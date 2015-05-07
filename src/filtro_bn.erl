%%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Laura M. Castro
%%% @copyright 2015
%%% @doc Documentation of the module `filtro_bn'.
%%%
%%% This program is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program. If not, see [http://www.gnu.org/licenses].
%%% @end
%%%-------------------------------------------------------------------
-module(filtro_bn).
-behaviour(tarea).

% Public API (according to spec).
-export([do/1]).

% Public API (required to start/stop components).
-export([programar/2]).

%% Private API (required for implementation purposes).
-export([loop/2,average/1,lightness/1,luminosity/1]).

% Internal macros
-define(TASKNAME, ?MODULE).
-define(DEFAULT_ALGORITHM, average).
-define(BYTES, 8).
-define(OFFSET, 4).
-define(PNG_HEADER, 137,$P,$N,$G,$\r,$\n,26,$\n).
-define(FIELD_LENGTH, 32).

%%-------------------------------------------------------------------
%% @doc Task creation and configuration.
%%      Receives a numerical ID (which will be used to determine the
%%      name of the output file), and the filename (string) of the
%%      image file to be turned B/N.
%%      It returns a reference to the task, ready to be executed.
%% @end
%%-------------------------------------------------------------------
-spec programar(ID :: integer(),
		FileName :: string()) -> {atom(),
					  pid()}.
programar(ID, FileName) ->
    {?TASKNAME,
     spawn(?MODULE, loop, [ID, FileName])}.

%%-------------------------------------------------------------------
%% @doc Executes a task.
%%      Receives a task (reference) to be executed, and triggers its
%%      execution. As an example, a task to be turned B/N configured
%%      with ID=73 and FileName="doc/html/erlang.png" will write the
%%      B/N version of the image in a file named `output-73.dat'.
%%      At the moment, only PNG images are supported. Beware that the
%%      contents of the `output-73.dat' file will represent a PNG
%%      image, despite the output file extension. This can be
%%      assessed in *nix systems by means of the `file' command.
%%      Alternatively, and to allow image display by external viewers
%%      that may be limited by image extension, the file can simply
%%      be manually renamed to `output-73.png'.
%% @end
%%-------------------------------------------------------------------
-spec do(Tarea :: pid()) -> ok.
do(Tarea) ->
    Tarea ! {do, self()},
    receive
	{done, Tarea} ->
	    ok
    end.



% Internal functions
% @private
loop(ID, FileName) ->
    receive
	{do, Who} ->
	    % this will break and the process will die if the task fails
	    {ok, BNImageData} = process_file(FileName),
	    OutputFileName = "output-" ++ erlang:integer_to_list(ID) ++ ".dat",
	    % this will break and the process will die if the file cannot be written
	    {ok, OutputFile} = file:open(OutputFileName, [write,binary,raw]),
	    [ok = file:write(OutputFile, Data) || Data <- BNImageData],
	    ok = file:close(OutputFile),
	    Who ! {done, self()}
    end.
	
process_file(FileName) ->
    % this will break and the process will die if the file cannot be read
    {ok, File} = file:open(FileName, [read,binary]),
    Z = zlib:open(),
    % first, read the PNG header
    {ok, FileHeader = <<?PNG_HEADER>>} = file:read(File, ?BYTES), 
    % then, there will be a number of chunks to go inspect to find the relevant data
    NewImageData = process_image(File, Z, [], []),
    ok = zlib:close(Z),
    ok = file:close(File),
    {ok, [FileHeader | NewImageData]}.

process_image(File, Z, Options, AccResult) ->
    {ok, ChunkHeader = <<Length:?FIELD_LENGTH, Type:?OFFSET/binary>>} = file:read(File, ?BYTES),
    % we proceed according to the type of chunk
    case Type of
	<<"IHDR">> -> % it should be the first chunk, but we do not enforce it
	    {Values, Chunks} = read(File, Z, Length, Type),
	    process_image(File, Z, Options ++ Values, AccResult ++ [ChunkHeader | Chunks]);
	<<"IDAT">> ->
	    RawImageData = read(File, Z, Length, Type),
	    {width, Width} = lists:keyfind(width, 1, Options),
	    {height, Height} = lists:keyfind(height, 1, Options),
	    {ok, NewRawImageData} = transform_data(Z, Height, Width, RawImageData),
	    NewCRC = zlib:crc32(Z, << Type/binary, NewRawImageData/binary >>),
	    process_image(File, Z, Options, AccResult ++ [ChunkHeader, NewRawImageData, <<NewCRC:?FIELD_LENGTH>>]);
	<<"IEND">> ->
	    lists:flatten(AccResult ++ [ChunkHeader]);
	_Else ->
	    process_image(File, Z, Options, AccResult ++ [ChunkHeader, skip(File, Length)])
    end.

skip(File, Length) ->
    {ok, SkippedChunk} = file:read(File, Length+?OFFSET),
    SkippedChunk.

read(File, _Z, Length, _Header = <<"IHDR">>) ->
    {ok, ChunkData} = file:read(File, Length),
    {ok, CRC} = file:read(File, ?OFFSET),
    << Width:?FIELD_LENGTH,
       Height:?FIELD_LENGTH,
       8:?BYTES,                              % only 1-byte color depth is supported
       2:?BYTES, _Rest/binary >> = ChunkData, % only RBG color type is supported
%    NewChunkData = << Width:?FIELD_LENGTH,
%		      Height:?FIELD_LENGTH,
%		      8:?BYTES,
%		      0:?BYTES, Rest/binary >>, % set color type to grayscale
%    NewCRC = zlib:crc32(Z, << Header/binary, NewChunkData/binary >>),
%    {[{width, Width}, {height, Height}], [NewChunkData, <<NewCRC:?FIELD_LENGTH>>]};
    {[{width, Width}, {height, Height}], [ChunkData, CRC]};
 read(File, _Z, Length, <<"IDAT">>) ->
    {ok, ChunkData} = file:read(File, Length),
    {ok, _CRC} = file:read(File, ?OFFSET),
    ChunkData.

transform_data(Z, Height, Width, RawData) ->
    ok = zlib:inflateInit(Z),
    % uncompress and process raw data
    Uncompressed = zlib:inflate(Z, RawData),
    ok = zlib:inflateEnd(Z),
    RawDataToCompress = transform(?DEFAULT_ALGORITHM, Height, Width, Uncompressed),
    ok = zlib:deflateInit(Z),
    % compress processed data
    Compressed = zlib:deflate(Z, RawDataToCompress, finish),
    ok = zlib:deflateEnd(Z),
    {ok, erlang:iolist_to_binary(Compressed)}.

transform(Algorithm, Height, Width, Raw) when is_list(Raw) ->
    RawList = lists:flatten([ binary:bin_to_list(S) || S <- Raw ]),
    io:format("Transforming (~p) each pixel of (~p x ~p image): ~p~n", [Algorithm,Width,Height,length(RawList)]),
    % we turn list into list of triplets, in chunks of Width
    RBGList = triplets(Height, Width, RawList),
    % we take values 3 by 3 (RGB) and calculate the new value for BN transformation
    BNList = lists:map(fun({A,B,C}) ->
			       erlang:apply(?MODULE, Algorithm, [{A,B,C}]);
			  (FT) -> % we skip the additional byte present at the beginning of every scanline
			       FT
		       end, RBGList),
    binary:list_to_bin(lists:flatten([ untripled(Item) || Item <- BNList])).

triplets(Height, Width, List) ->
    do_triplets(Height, Width, 3*Width+1, List, []).

do_triplets(0, _Width, _LWidth, [], Acc) ->
    lists:reverse(Acc);
do_triplets(Height, Width, 0, Rest, Acc) ->
    do_triplets(Height-1, Width, 3*Width+1, Rest, Acc);
do_triplets(Height, Width, LWidth, [H | Rest], Acc) when (Width*3+1 == LWidth) ->
    do_triplets(Height, Width, LWidth-1, Rest, [H | Acc]);
do_triplets(Height, Width, LWidth, [A,B,C | Rest], Acc) ->
    do_triplets(Height, Width, LWidth-3, Rest, [{A,B,C} | Acc]).

untripled({A,B,C}) ->
    [A,B,C];
untripled(FT) ->
    FT.

average({A,B,C}) ->
    Avg = (A + B + C) div 3,
    {Avg, Avg, Avg}.

lightness({A,B,C}) ->
    Avg = (max(max(A,B),max(B,C)) + min(min(A,B),min(B,C))) div 2,
    {Avg, Avg, Avg}.

luminosity({A,B,C}) ->
    {trunc(0.21 * A),
     trunc(0.72 * B),
     trunc(0.07 * C)}.
