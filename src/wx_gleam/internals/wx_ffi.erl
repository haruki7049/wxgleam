%%% @doc Foreign Function Interface (FFI) module for wxWidgets bindings.
%%%
%%% This module provides the Erlang implementation of wxWidgets functionality
%%% that is exposed to Gleam through FFI. It handles the conversion between
%%% Gleam types (like binaries) and Erlang types (like lists) and wraps the
%%% underlying wx Erlang module.
%%%
%%% @end

-module(wx_ffi).
-export([init_wx/0,
         create_frame/2,
         show_frame/1,
         create_button/3,
         connect_close_event/1,
         apply_message_handler/1,
         destroy/0]).

% Include the wxWidgets header file which defines macros and records
-include_lib("wx/include/wx.hrl").


%%% @doc Initialize the wxWidgets application and return the App object.
%%%
%%% This function starts the wx application and creates a new wx server instance.
%%% It must be called before any other wx functions.
%%%
%%% @returns `{ok, WxApp}' on success, or `{error, Reason}' if initialization fails
%%% @end
init_wx() ->
    % Check the return value of application:start to handle already-started case
    case application:start(wx) of
        ok ->
            % Call wx:new only if application start was successful
            WxApp = wx:new(),
            {ok, WxApp};
        {error, Reason} ->
            % If failed, return the reason to Gleam
            {error, Reason}
    end.


%%% @doc Create a new frame (window) with the specified title.
%%%
%%% This function creates a wxFrame with a default size of 400x300 pixels.
%%% The title is converted from Gleam's binary string format to Erlang's
%%% list format.
%%%
%%% @param WxApp The wx application instance
%%% @param Title The window title as a binary string
%%% @returns `{ok, Frame}' with the created frame reference
%%% @end
create_frame(WxApp, Title) ->
    % Convert Gleam's String (Binary) to Erlang's String (List) for the title
    TitleList = binary_to_list(Title),
    Frame = wxFrame:new(WxApp,
                        ?wxID_ANY,
                        TitleList,
                        [{size, {400, 300}}]),
    {ok, Frame}.


%%% @doc Make a frame visible on the screen.
%%%
%%% This function shows the specified frame, making it visible to the user.
%%%
%%% @param Frame The frame reference to show
%%% @returns `ok'
%%% @end
show_frame(Frame) ->
    wxFrame:show(Frame),
    ok.


%%% @doc Create a button widget with the specified label.
%%%
%%% This function creates a wxButton as a child of the specified frame.
%%% The label is converted from Gleam's binary string format to Erlang's
%%% list format.
%%%
%%% @param Frame The parent frame that will contain the button
%%% @param ID The widget ID (use -1 for wxID_ANY)
%%% @param Label The button label as a binary string
%%% @returns `{ok, Button}' with the created button reference
%%% @end
create_button(Frame, ID, Label) ->
    % Convert Gleam's String (Binary) to Erlang's String (List) for the label
    LabelList = binary_to_list(Label),
    % Create a button as a child of the Frame
    Button = wxButton:new(Frame, ID, [{label, LabelList}]),
    {ok, Button}.


%%% @doc Connect a close event handler to a frame.
%%%
%%% This function sets up the frame to send close_window events, which can
%%% be received and handled by the application.
%%%
%%% @param Frame The frame to connect the close event to
%%% @returns `ok'
%%% @end
connect_close_event(Frame) ->
    wxFrame:connect(Frame, close_window),
    ok.


%%% @doc Wait for and handle messages, including close events.
%%%
%%% This function blocks until a close_window event is received. Any other
%%% messages received while waiting are passed to the provided Handler function.
%%% After handling non-close messages, it recursively waits for more messages.
%%%
%%% @param Handler A function that will be called with any non-close messages
%%% @returns `ok' when a close event is received
%%% @end
apply_message_handler(Handler) ->
    receive
        Message ->
            % Call the Gleam function (Handler) with the message
            Handler(Message),
            % Continue waiting recursively
            apply_message_handler(Handler)
    end.


%%% @doc Clean up and destroy the wxWidgets application.
%%%
%%% This function destroys the wx server instance and should be called
%%% when the application is shutting down to properly clean up resources.
%%%
%%% @returns `ok'
%%% @end
destroy() ->
    wx:destroy(),
    ok.
