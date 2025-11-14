-module(wx_ffi).
-export([init_wx/0,
         create_frame/2,
         show_frame/1,
         create_button/3,
         connect_close_event/1,
         await_close_message/0,
         destroy/0]).

% Here we include the wx.hrl file
-include_lib("wx/include/wx.hrl").


% Initialize wx and return the App object
init_wx() ->
    % --- Fix: Check the return value of application:start ---
    case application:start(wx) of
        ok ->
            % Call wx:new only if successful
            WxApp = wx:new(),
            {ok, WxApp};
        {error, Reason} ->
            % If failed, return the reason to Gleam
            {error, Reason}
    end.


% Create a frame
% Gleam's String (Binary) is converted to Erlang's String (List) for the title
create_frame(WxApp, Title) ->
    TitleList = binary_to_list(Title),
    Frame = wxFrame:new(WxApp,
                        ?wxID_ANY,
                        TitleList,
                        [{size, {400, 300}}]),
    {ok, Frame}.  % Return the frame reference


% Show the frame
show_frame(Frame) ->
    wxFrame:show(Frame),
    ok.


% We accept the parent (Frame) and a Label
create_button(Frame, ID, Label) ->
    LabelList = binary_to_list(Label),
    % Create a button as a child of the Frame
    Button = wxButton:new(Frame, ID, [{label, LabelList}]),
    {ok, Button}.


% --- NEW: Send the window close event to the specified PID ---
connect_close_event(Frame) ->
    wxFrame:connect(Frame, close_window),
    ok.


% --- NEW: Block until the close event message arrives ---
await_close_message() ->
    receive
        % Message matching wxEVT_CLOSE_WINDOW
        #wx{event = #wxClose{}} ->
            ok;  % Exit the loop
        _OtherMessage ->
            % Ignore other messages (such as wx events) and continue waiting
            await_close_message()
    end.


destroy() ->
    wx:destroy(),
    ok.
