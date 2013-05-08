-module(parse).

-export([tuple_b_to_s/1]).

tuple_b_to_s([]) -> [];

tuple_b_to_s(T) when is_tuple(T) ->
	T2 = tuple_b_to_s(tuple_to_list(T)),
	list_to_tuple(T2);

tuple_b_to_s(T) when is_atom(T) ->
	T;

tuple_b_to_s(T) when is_binary(T) ->
	binary_to_list(T);

tuple_b_to_s([A|B]=T) when is_list(T) ->
	case io_lib:printable_list(T) of
	true -> T;
	false ->
		case B of
		[] ->
			[tuple_b_to_s(A)];
		_ ->
			[tuple_b_to_s(A)]++tuple_b_to_s(B)
		end
	end.
