%%% @doc Foreign Function Interface (FFI) module for wxWidgets bindings.
%%%
%%% This module provides the Erlang implementation of wxWidgets functionality
%%% that is exposed to Gleam through FFI. It serves as the bridge between
%%% Gleam's type system and Erlang's wx module.
%%%
%%% <h2>Key Responsibilities</h2>
%%% <ul>
%%%   <li>Converting between Gleam types (binaries) and Erlang types (lists)</li>
%%%   <li>Wrapping Erlang's wx module functions with Gleam-friendly signatures</li>
%%%   <li>Providing proper error handling with tagged tuples</li>
%%%   <li>Managing wx application lifecycle</li>
%%% </ul>
%%%
%%% <h2>Type Conversions</h2>
%%% <p>Gleam strings are binaries, while Erlang's wx module expects lists.
%%% This module performs the necessary conversions using `binary_to_list/1'.</p>
%%%
%%% <h2>Usage</h2>
%%% <p>This module is not meant to be called directly from Gleam. Instead, use
%%% the public API in the `wx_gleam' module, which wraps these functions with
%%% more user-friendly types.</p>
%%%
%%% @end

-module(wx_ffi).
-export([init_wx/0,
         create_frame/2,
         show_frame/1,
         create_button/3,
         connect_close_event/1,
         await_close_message/1,
         destroy/0]).

% Include the wxWidgets header file which defines macros and records
-include_lib("wx/include/wx.hrl").


%%% @doc Initialize the wxWidgets application and return the App object.
%%%
%%% This function starts the wx Erlang application and creates a new wx server
%%% instance. The wx application must be started before any GUI components can
%%% be created.
%%%
%%% <h3>Process</h3>
%%% <ol>
%%%   <li>Start the wx Erlang application using `application:start(wx)'</li>
%%%   <li>If successful, create a new wx server with `wx:new()'</li>
%%%   <li>Return the wx server reference wrapped in `{ok, WxApp}'</li>
%%%   <li>If startup fails, return `{error, Reason}'</li>
%%% </ol>
%%%
%%% @returns `{ok, WxApp}' on success, where WxApp is the wx server reference,
%%%          or `{error, Reason}' if the wx application cannot be started
%%%          (e.g., if it's already running or if wxWidgets is not available)
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
%%% This function creates a wxFrame (top-level window) with a default size of
%%% 400x300 pixels. The frame is initially hidden and must be shown with
%%% `show_frame/1'.
%%%
%%% <h3>Type Conversion</h3>
%%% <p>The title parameter is a Gleam string (binary) which is converted to an
%%% Erlang string (list) using `binary_to_list/1' before being passed to wxFrame.</p>
%%%
%%% <h3>Frame Properties</h3>
%%% <ul>
%%%   <li>ID: wxID_ANY (automatically assigned)</li>
%%%   <li>Size: 400x300 pixels</li>
%%%   <li>Initial state: Hidden</li>
%%%   <li>Has title bar with system buttons (minimize, maximize, close)</li>
%%% </ul>
%%%
%%% @param WxApp The wx application instance from `init_wx/0'
%%% @param Title The window title as a binary string (Gleam String)
%%% @returns `{ok, Frame}' with the created frame reference. The frame is
%%%          guaranteed to be valid and ready to use.
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
%%% This function calls `wxFrame:show/1' to make the specified frame visible
%%% to the user. The frame will appear on screen at a position determined by
%%% the operating system's window manager.
%%%
%%% @param Frame The frame reference to show (obtained from `create_frame/2')
%%% @returns `ok' - This operation always succeeds for a valid frame reference
%%% @end
show_frame(Frame) ->
    wxFrame:show(Frame),
    ok.


%%% @doc Create a button widget with the specified label.
%%%
%%% This function creates a wxButton as a child widget of the specified frame.
%%% The button will be visible within the frame's client area and can be clicked
%%% by the user.
%%%
%%% <h3>Type Conversion</h3>
%%% <p>The label parameter is a Gleam string (binary) which is converted to an
%%% Erlang string (list) using `binary_to_list/1' before being passed to wxButton.</p>
%%%
%%% <h3>Button Properties</h3>
%%% <ul>
%%%   <li>Parent: The specified frame</li>
%%%   <li>ID: As specified (use -1 for wxID_ANY to auto-assign)</li>
%%%   <li>Label: The text displayed on the button</li>
%%%   <li>Size: Automatically sized to fit the label</li>
%%% </ul>
%%%
%%% @param Frame The parent frame that will contain the button
%%% @param ID The widget ID (use -1 for wxID_ANY to let wx auto-assign an ID)
%%% @param Label The button label as a binary string (Gleam String)
%%% @returns `{ok, Button}' with the created button reference. The button is
%%%          guaranteed to be valid and ready to use.
%%% @end
create_button(Frame, ID, Label) ->
    % Convert Gleam's String (Binary) to Erlang's String (List) for the label
    LabelList = binary_to_list(Label),
    % Create a button as a child of the Frame
    Button = wxButton:new(Frame, ID, [{label, LabelList}]),
    {ok, Button}.


%%% @doc Get the default list of close event subtypes.
%%%
%%% This function returns all the wxCloseEventType subtypes that should be
%%% connected by default. These events cover different scenarios when a
%%% window or application is being closed.
%%%
%%% <h3>Event Types</h3>
%%% <ul>
%%%   <li>`close_window' - User clicked close button or pressed Alt+F4</li>
%%%   <li>`end_session' - System is shutting down or logging out</li>
%%%   <li>`query_end_session' - System is querying if it's safe to close</li>
%%% </ul>
%%%
%%% @returns A list of event type atoms
%%% @end
default_close_subtypes() ->
    [close_window, end_session, query_end_session].


%%% @doc Connect multiple event types to a frame.
%%%
%%% This helper function connects each event type in the provided list to
%%% the specified frame. All events will be sent to the calling process.
%%%
%%% @param Frame The frame to connect events to
%%% @param Events A list of event type atoms to connect
%%% @returns `ok' after all events are connected
%%% @end
connect_events(Frame, Events) ->
    lists:foreach(fun(E) -> wxFrame:connect(Frame, E) end, Events),
    ok.


%%% @doc Connect close event handler(s) to a frame.
%%%
%%% This function sets up the frame to send close events to the calling
%%% process when the user attempts to close the window. These events can be
%%% received and handled using `await_close_message/1'.
%%%
%%% <h3>Usage Patterns</h3>
%%% <ul>
%%%   <li>`connect_close_event(Frame)' - Connects all close subtypes (default)</li>
%%%   <li>`connect_close_event({Frame, Events})' - Connects specific event types</li>
%%% </ul>
%%%
%%% <h3>Event Types</h3>
%%% <p>By default, this function connects all wxCloseEventType subtypes:
%%% close_window, end_session, and query_end_session. To connect only specific
%%% event types, pass a tuple with the frame and a list of event atoms.</p>
%%%
%%% <h3>Event Handling</h3>
%%% <p>After calling this function, the frame will send wx event records to
%%% the Erlang process that called this function. The events are sent when
%%% the user clicks the close button, when the system is shutting down, or
%%% when other close-related events occur.</p>
%%%
%%% @param Frame The frame to connect the close event to (from `create_frame/2'),
%%%        or a tuple `{Frame, Events}' where Events is a list of event atoms
%%% @returns `ok' - The event connection always succeeds for a valid frame
%%% @end
connect_close_event({Frame, Events}) when is_list(Events) ->
    connect_events(Frame, Events);
connect_close_event(Frame) ->
    % Backward compatible: connect all close subtypes by default
    connect_events(Frame, default_close_subtypes()),
    ok.


%%% @doc Wait for and handle messages, including close events.
%%%
%%% This function blocks the calling process until a close event is received.
%%% It implements a message loop that can handle other messages while waiting
%%% for the close event. When a close event is detected, it normalizes the
%%% event to a simple `{close, Type}' tuple and passes it to the handler before
%%% returning.
%%%
%%% <h3>Message Loop Behavior</h3>
%%% <ol>
%%%   <li>Enter a receive block to wait for messages</li>
%%%   <li>If a wxClose event is received:
%%%     <ul>
%%%       <li>Extract the event Type (close_window, end_session, or query_end_session)</li>
%%%       <li>Call Handler with `{close, Type}' tuple for Gleam-side handling</li>
%%%       <li>Return `ok' and exit the loop</li>
%%%     </ul>
%%%   </li>
%%%   <li>If any other message is received:
%%%     <ul>
%%%       <li>Pass it to the Handler function</li>
%%%       <li>Recursively call `await_close_message/1' to continue waiting</li>
%%%     </ul>
%%%   </li>
%%% </ol>
%%%
%%% <h3>Close Event Normalization</h3>
%%% <p>This function normalizes wxClose records into simple `{close, Type}' tuples,
%%% making them easier to handle in Gleam code. The Type atom indicates which
%%% close event subtype was received (close_window, end_session, or query_end_session).</p>
%%%
%%% <h3>Use Cases</h3>
%%% <ul>
%%%   <li>Keeping the application alive until the user closes a window</li>
%%%   <li>Handling different types of close events (user close vs system shutdown)</li>
%%%   <li>Processing other messages (e.g., button clicks) while waiting</li>
%%%   <li>Implementing custom message handling logic</li>
%%% </ul>
%%%
%%% @param Handler A Gleam function that will be called with messages. For close
%%%                events, receives `{close, Type}' tuple. For other messages,
%%%                receives the message as a dynamic value. Should return Nil.
%%% @returns `ok' when a close event is received and handled
%%% @end
await_close_message(Handler) ->
    receive
        % Message matching wxClose event - normalize and pass to handler, then exit
        #wx{event = #wxClose{type = Type}} ->
            % Normalize the close event to {close, Type} for Gleam-side handling
            Handler({close, Type}),
            ok;
        % Any other message - pass to the handler and continue waiting
        OtherMessage ->
            % Call the Gleam function (Handler) with the message
            Handler(OtherMessage),
            % Continue waiting recursively
            await_close_message(Handler)
    end.


%%% @doc Clean up and destroy the wxWidgets application.
%%%
%%% This function destroys the wx server instance and releases all resources
%%% associated with the wx application. It should be called when the application
%%% is shutting down.
%%%
%%% <h3>Cleanup Process</h3>
%%% <p>Calls `wx:destroy/0' which:</p>
%%% <ul>
%%%   <li>Destroys all wx objects created by this wx server</li>
%%%   <li>Terminates the wx server process</li>
%%%   <li>Releases GUI resources</li>
%%%   <li>Prevents memory leaks</li>
%%% </ul>
%%%
%%% <h3>Important Notes</h3>
%%% <ul>
%%%   <li>Should be called exactly once at application shutdown</li>
%%%   <li>After calling this, `init_wx/0' must be called again to use wx</li>
%%%   <li>Automatically handled by the `with_app' wrapper in Gleam</li>
%%% </ul>
%%%
%%% @returns `ok' - Cleanup always succeeds
%%% @end
destroy() ->
    wx:destroy(),
    ok.
