-module(framework_kamer).
-export([start/0,checkwalls/4, checkroom/3, checkdoublescore/4, checkrooms/3, randomwall/1,setwall/4, printrooms/1,
display/2, playloop/0,buildwall/2, test/1]).
-define(SIZE, 60).
-define(HLINES, 30).
-define(HSIZE, 5).
-define(VSIZE, 6).

%%% Setup a random start configuration for testing purposes
start() ->
    % Set up Rooms
    Rooms = array:new([{default, false}, {size, ?SIZE}]),
    test(Rooms).
    % Create the two players

    % Send message to player 1
    % This message could look like:
    % pid_player_1!{..., ..., *any other variables you
    % want to pass along between the players*}


%%% Contains the whole game(loop) for each player.
playloop() ->
    % Receives the given message {..., ..., ...} from the
    %other player
    receive {} ->
        % Make a move (use the checkrooms function for this)

        % Print rooms and score to the terminal

        % Send the updated information to the other player

        % Call this function again, so that the player can
        % receive again.
        % Note: be sure to check when it is gameover
        % (stop calling the fuction again)
        % (be sure to set the magic number and the score!)
        Magic_number = 0,
        Score = 0,
        if  Score < Magic_number ->
	        playloop();
            % Else, print game over and end
            true ->
            io:format("Game Over",[])
        end
    end.


%%% Checks whether Rooms[I,J] can get a score and sets a
%%% random wall otherwise. Always start checking from index = 0.
%%% Score is the current score.
%%% Returns the updated Rooms and Score.
checkrooms(Rooms, Index, Score) ->
    % Get I and J from Index
    I = Index rem 5,
    J = (Index - (Index rem 5)) / 5,

    % Check whether room I, J can be completed with a wall
    % (use the checkroom function for this)
    direction = checkroom(Rooms, I, J),

    if  direction /= fail ->
        % If this is the case, check whether setting this
        % wall also completes another room
        % (use the checkdoublescore function).
        case checkdoublescore(Rooms, I, J, direction) of
            true -> score = Score + 2;
            false -> score = Score + 1
        end,

        % Print the new move and new score
        io:format(direction),
        io:format(Score),

        % Set the new wall (use the setwall function) and
        % return the Rooms and Score
        setwall(Rooms, I, J, direction);

        true ->
        % If this is not the case, check the next room
        % (Index + 1). However, if all the indices have been
        % checked (= no room can be completed by placing a wall),
        % place a random wall (instead of checking the next room!).


        % (replace/remove true here)
        true
    end.

%%% Check to see whether a room I, J can be completed with a wall.
checkroom(Rooms, I, J) ->
    % For each of the sides (left, top, right and bottom) of
    % room I, J:
    % Check if placing a wall will complete the room
    % (use the checkwalls function four times).

    A = checkwalls(Rooms, I, J, left),
    B = checkwalls(Rooms, I, J, top),
    C = checkwalls(Rooms, I, J, right),
    D = checkwalls(Rooms, I, J, bottom),

    % If one of the sides completes the room, return that side.
    case true of
        A -> left;
        B -> top;
        C -> right;
        D -> bottom;
        _ -> fail
    end.

%%% Functions for setting and checking rooms. Should start with
%%% an array Rooms from array:new([{default:false}, {size,?SIZE}]).
%%% Check to see if a wall (top, bottom, left or right) completes
%%% a room.
%%% Return true if placing a wall in direction Wall will
%%% complete a room.
checkwalls(Rooms, I, J, Wall) ->
    Top = array:get((J * 11 + I), Rooms),
    Bottom = array:get((J * 11 + I + 11), Rooms),
    Left = array:get((J * 11 + I + 5), Rooms),
    Right = array:get((J * 11 + I + 6), Rooms),
    % Check if placing a wall in direction Wall will complete
    % a room.
    case Wall of
        left ->
            case Top and Bottom and Right of
                true -> true;
                false -> false
            end;
        right ->
            case Top and Left and Bottom of
                true -> true;
                false -> false
            end;
        bottom ->
            case Top and Left and Right of
                true -> true;
                false -> false
            end;
        top ->
            case Right and Left and Bottom of
                true -> true;
                false -> false
            end
    end.


%%% Checks to see if setting a wall in I,J completes a neighboring
%%% room.
checkdoublescore(Rooms, I, J, Wall) ->
    case Wall of
        left -> checkwalls(Rooms, I - 1, J, right);
        right -> checkwalls(Rooms, I + 1, J, left);
        top -> checkwalls(Rooms, I, J - 1, bottom);
        bottom -> checkwalls(Rooms, I, J + 1, top)
    end.


%%% Sets a wall in Room [I,J], in direction Wall
setwall(Rooms, I, J, Wall) ->
    % Call the helperfunction buildwall with the correct arguments
    % (these depend on the variable Wall)
    case Wall of
        left -> index =

    buildwall(Rooms, index)
    %(replace/remove true here)
    true.

%%% Helperfunction for setwall; checks before setting.
buildwall(Rooms,Index) ->
    % Check if the position in the array has not already been
    % set before setting it.


    %(replace/remove true here)
    true.

%%% Given an index Start, runs clockwise to find an unset wall
%%% and sets it
%%% (as the room Start might already be set).
%%% Safeguard it, so it never takes more than ?SIZE steps.
%%% Returns the updated Rooms.
setnextwall(Rooms, Start, 0) ->
    true;
setnextwall(Rooms, Start, Steps) ->
    Room = array:get(Start rem ?SIZE, Rooms),
    case Room of
        true -> setnextwall(Rooms, Start + 1, Steps - 1);
        false -> array:set(Start rem ?SIZE, true, Rooms)
    end.

%%% Draws a random number mod ?SIZE and tries to set the
%%% corresponding wall (use the setnextwall function here).
%%% Returns the updated Rooms
randomwall(Rooms) ->
    Random = random:uniform(?SIZE),
    setnextwall(Rooms, 0, ?SIZE).

%%% Prints the rooms
printrooms(Rooms) ->
    display(Rooms, 0).

display(Rooms, 60) ->
    exit(q);
display(Rooms, In) ->
    Room = array:get(In, Rooms),
    Rm = (In + 1) rem 11,
    % Get Room
    Room = array:get(In, Rooms),
    % Give first 12 elements a number from 0 to 11
    Rm = (In + 1) rem 11,

    case Rm < 6 andalso Rm > 0 of
        true ->
            case Room of
                false -> io:format("+   ");
                true -> io:format("+---")
            end;
        false ->
            case Room of
                false -> io:format("   ");
                true -> io:format("|   ")
            end
    end,

    case Rm of
        0 -> io:format("~n");
        5 -> io:format("+~n");
        _ -> true
    end,
    if
        In == 60 -> display(Rooms, 60);
        true ->  display(Rooms, In + 1)
    end.

test(Rooms) ->
    Rooms_new = randomwall(Rooms),
    Dir = checkroom(Rooms_new, 0, 0),
    io:format("~s~n", [Dir]),
    printrooms(Rooms_new),
    test(Rooms_new).
