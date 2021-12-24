-module(erl_qoi).

-export([]).

-compile(export_all).

-define(QOI_MAGIC, "qoif").
-define(QOI_HEADER_SIZE, 14).
-define(QOI_END_MARKER, <<0,0,0,0,0,0,0,1>>).

-define(QOI_DIFF_BIAS, 2).
-define(QOI_LUMA_GREEN_BIAS, 32).
-define(QOI_LUMA_BIAS, 8).
-define(QOI_RUN_BIAS, (-1)).

-include("include/erl_qoi.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc parse QOI header.
-spec parse_header(binary()) -> {ok, #qoi_header{}}.
parse_header(B) when byte_size(B) >= ?QOI_HEADER_SIZE ->
	<<?QOI_MAGIC,
		Width:32/unsigned-big,
		Height:32/unsigned-big,
		Channels:8/unsigned,
		ColorSpace:8/unsigned,
		_Rest/binary>> = B,
	{ok, #qoi_header{width=Width, height=Height, channels=Channels, colorspace=ColorSpace}}.


%% ----------------------------------
%% @doc return the pixel type assuming the first byte of the argument is the start of a chunk.
%% @end
%% ----------------------------------
-spec chunk_type(binary()) -> atom().
chunk_type(<<254:8/unsigned, _Rest/binary>>)  -> 'QOI_OP_RGB';    %% 8 Bit chunks
chunk_type(<<255:8/unsigned, _Rest/binary>>)  -> 'QOI_OP_RGBA';
chunk_type(<<0:2/unsigned, _Rest/bitstring>>) -> 'QOI_OP_INDEX';  %% 2-Bit chunks
chunk_type(<<1:2/unsigned, _Rest/bitstring>>) -> 'QOI_OP_DIFF';
chunk_type(<<2:2/unsigned, _Rest/bitstring>>) -> 'QOI_OP_LUMA';
chunk_type(<<3:2/unsigned, _Rest/bitstring>>) -> 'QOI_OP_RUN'.


%% could just put the binary match in the head
decode_chunk('QOI_OP_RGB', B) ->
	<<_:1/binary,Red:8/unsigned,Green:8/unsigned,Blue:8/unsigned,Rest/binary>> = B,
	{ok, #qoi_op_rgb{r=Red,g=Green,b=Blue,a=255}, Rest};
decode_chunk('QOI_OP_RGBA', B) ->
	<<_:1/binary,Red:8/unsigned,Green:8/unsigned,Blue:8/unsigned,Alpha:8/unsigned,Rest/binary>> = B,
	{ok, #qoi_op_rgba{r=Red,g=Green,b=Blue,a=Alpha}, Rest};
decode_chunk('QOI_OP_INDEX', B) ->
	<<_:2/bitstring,Index:6/unsigned,Rest/binary>> = B,
	{ok, #qoi_op_index{index=Index}, Rest};
decode_chunk('QOI_OP_DIFF', B) ->
	<<_:2/bitstring,DR:2/unsigned,DG:2/unsigned,DB:2/unsigned,Rest/binary>> = B,
	{ok, #qoi_op_diff{dr=DR,dg=DG,db=DB}, Rest};
decode_chunk('QOI_OP_LUMA', B) ->
	<<_:2/bitstring,DG:6/unsigned,DRG:4/unsigned,DBG:2/unsigned,Rest/binary>> = B,
	{ok, #qoi_op_luma{dg=DG, drg=DRG, dbg=DBG}, Rest};
decode_chunk('QOI_OP_RUN', B) ->
	<<_:2/bitstring,Run:6/unsigned,Rest/binary>> = B,
	{ok, #qoi_op_run{run=Run}, Rest}.

decode_chunk(B) ->
	decode_chunk(chunk_type(B), B).


-spec make_prev_array() -> tuple().
make_prev_array() ->
	array:new([{size, 64}, {fixed, true}, {default, false}]).


%% index_position = (r * 3 + g * 5 + b * 7 + a * 11) % 64
-spec index_position(byte(), byte(), byte(), byte()) -> byte().
index_position(R, G, B, A) ->
	((R*3) + (G*5) + (B*7) + (A*11)) rem 64.


decode(B) ->
	{ok, Header} = parse_header(B),
	<<_Header:?QOI_HEADER_SIZE/binary,Body/binary>> = B,
	{ok, Body} = decode_body(Body),
	{ok, Header, Body}.

decode_body(B) ->
	PrevArray = make_prev_array(),
	PrevPixel = #qoi_pixel{r=0,g=0,b=0,a=255},
	Acc = array:new(),
	Idx = 0,
	decode_body(B, PrevArray, PrevPixel, Idx, Acc).

%% we've seen the end
decode_body(<<>>, _, _, _, _) -> error_out_of_bytes;
decode_body(?QOI_END_MARKER, _, _, _, Acc) -> {ok, Acc};

%% hot loop - interpret each chunk
decode_body(B, PrevArray, PrevPixel, Idx, Acc) ->
	{ok, Chunk, Rest} = decode_chunk(B),
	case Chunk of
		#qoi_op_rgb{r=R,g=G,b=B,a=A} ->
			CurrPixel = {R,G,B,A},
			NewPrevArray = array:set(index_position(R,G,B,A), CurrPixel, PrevArray),
			NewAcc = array:set(Idx, CurrPixel, Acc),
			decode_body(Rest, NewPrevArray, CurrPixel, Idx+1, NewAcc); %% tail call

		#qoi_op_rgba{r=R,g=G,b=B,a=A} ->
			CurrPixel = {R,G,B,A},
			NewPrevArray = array:set(index_position(R,G,B,A), CurrPixel, PrevArray),
			NewAcc = array:set(Idx, CurrPixel, Acc),
			decode_body(Rest, NewPrevArray, CurrPixel, Idx+1, NewAcc); %% tail call

		#qoi_op_index{index=I} ->
			{R,G,B,A} = array:get(I, PrevArray),
			NewAcc = array:set(Idx, {R,G,B,A}, Acc),
			decode_body(Rest, PrevArray, {R,G,B,A}, Idx+1, NewAcc); %% tail call

		#qoi_op_diff{dr=DR,dg=DG,db=DB} ->
			{Rp,Gp,Bp,A} = PrevPixel, %% previous pixel values (A is copied to the current color)
			{R,G,B} = calc_pixel_from_diff(Rp,Gp,Bp,DR,DG,DB),
			CurrPixel = {R,G,B,A},
			NewPrevArray = array:set(index_position(R,G,B,A), CurrPixel, PrevArray),
			NewAcc = array:set(Idx, {R,G,B,A}, Acc),
			decode_body(Rest, NewPrevArray, CurrPixel, Idx+1, NewAcc); %% tail call
		#qoi_op_luma{dg=DG, drg=DR, dbg=DB} ->
			{Rp,Gp,Bp,A} = PrevPixel, %% previous pixel values (A is copied to the current color)
			{R,G,B} = calc_pixel_luma(Rp,Gp,Bp,DR,DG,DB),
			CurrPixel = {R,G,B,A},
			NewPrevArray = array:set(index_position(R,G,B,A), CurrPixel, PrevArray),
			NewAcc = array:set(Idx, {R,G,B,A}, Acc),
			decode_body(Rest, NewPrevArray, CurrPixel, Idx+1, NewAcc); %% tail call;
		#qoi_op_run{run=RunBiased} ->
			%% Run = RunBiased - ?QOI_RUN_BIAS,
			true
	end.

%% TODO handle values greater than 511? Right now we don't rem by 256
%% byte() type indicates that we only guarantee correct clamping on values < 512 (max(byte()) + max(byte()))
-spec clamp_byte(byte()) -> byte().
clamp_byte(X) when X < 0 -> 256+X;
clamp_byte(X) when X > 255 -> X-256;
clamp_byte(X) -> X.

%% TODO validate this math
-spec calc_pixel_from_diff(byte(), byte(), byte(), byte(), byte(), byte()) -> {byte(), byte(), byte()}.
calc_pixel_from_diff(Rp,Gp,Bp,DR,DG,DB) ->
	R = clamp_byte(Rp + (DR - ?QOI_DIFF_BIAS)),
	G = clamp_byte(Gp + (DG - ?QOI_DIFF_BIAS)),
	B = clamp_byte(Bp + (DB - ?QOI_DIFF_BIAS)),
	{R, G, B}.

%% TODO validate this math
-spec calc_pixel_luma(byte(), byte(), byte(), byte(), byte(), byte()) -> {byte(), byte(), byte()}.
calc_pixel_luma(Rp,Gp,Bp,DR,DG,DB) ->
	GDiff = DG - ?QOI_LUMA_GREEN_BIAS,
	G = clamp_byte(Gp + GDiff),
	R = clamp_byte(Rp + (GDiff + (DR - ?QOI_LUMA_BIAS))),
	B = clamp_byte(Bp + (GDiff + (DB - ?QOI_LUMA_BIAS))),
	{R, G, B}.


%% API
append_run(0, _, Acc) -> Acc;
append_run(Count, Term, Acc) ->
	?debugVal(Count),
	?debugVal(Term),
	?debugVal(Acc),
	NewAcc = array:set(array:size(Acc), Term, Acc),
	append_run(Count - 1, Term, NewAcc).

%% ----------------------------------
%% TESTS
%% ----------------------------------
-ifdef(EUNIT).

%% from dice.qoi test image
dice_header() ->
	<<16#71, 16#6f, 16#69, 16#66, 16#00, 16#00, 16#03, 16#20, 
	16#00, 16#00, 16#02, 16#58, 16#04, 16#00>>.

parse_header_test() ->
	{ok, Qoi} = parse_header(dice_header()),
	?assertEqual(Qoi#qoi_header.width, 800),
	?assertEqual(Qoi#qoi_header.height, 600), 
	?assertEqual(Qoi#qoi_header.channels, 4),
	?assertEqual(Qoi#qoi_header.colorspace, 0).

parse_pixel_type_test() ->
	?assertEqual(chunk_type(<<16#fe>>), 'QOI_OP_RGB'),
	?assertEqual(chunk_type(<<16#ff>>), 'QOI_OP_RGBA'),
	?assertEqual(chunk_type(<<16#00>>), 'QOI_OP_INDEX').

clamp_byte_test() ->
	?assertEqual(0, clamp_byte(0)),
	?assertEqual(0, clamp_byte(256)),
	?assertEqual(1, clamp_byte(250 + 7)),
	?assertEqual(253, clamp_byte(10 - 13)).

calc_pixel_from_diff_test() ->
	%% cover the 4 cases of any given delta (2 bit, 4 options: 0, 1, 2, 3)
	?assertEqual({8,9,10}, calc_pixel_from_diff(10,10,10,0,1,2)),
	?assertEqual({11,8,8}, calc_pixel_from_diff(10,10,10,3,0,0)),

	%% test for correct negative wrap around
	?assertEqual({255,254,255}, calc_pixel_from_diff(0,0,0,1,0,1)),

	%% test for correct positive wrap around
	?assertEqual({0,0,0}, calc_pixel_from_diff(255,255,255,3,3,3)).

append_run_test() ->
	Empty = array:new([{size, 0}, {default, 1}, {fixed, false}]),
	One = array:new([{size, 1}, {default, 1}, {fixed, false}]),
	Five = array:new([{size, 5}, {default, 1}, {fixed, false}]),
	Ten = array:new([{size, 10}, {default, 1}, {fixed, false}]),

	%?assertEqual(Empty, append_run(0, 1, Empty)),
	?assertEqual(array:fix(One), array:fix(append_run(1, 1, Empty))),
	%?assertEqual(Five, append_run(5, 1, Empty)),
	%?assertEqual(Ten, append_run(5, 1, Five)).
	?debugMsg("DONE").

-endif.