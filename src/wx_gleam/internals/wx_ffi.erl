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
    % --- 修正: application:start の戻り値をチェック ---
    case application:start(wx) of
        ok ->
            % 成功した場合のみ wx:new を呼ぶ
            WxApp = wx:new(),
            {ok, WxApp};
        {error, Reason} ->
            % 失敗した場合、理由をGleamに返す
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


% --- NEW: ウィンドウのクローズイベントを指定されたPIDに送信する ---
connect_close_event(Frame) ->
    wxFrame:connect(Frame, close_window),
    ok.


% --- NEW: クローズイベントのメッセージが来るまでブロックする ---
await_close_message() ->
    receive
        % wxEVT_CLOSE_WINDOW に一致するメッセージ
        #wx{event = #wxClose{}} ->
            ok;  % ループを抜ける
        _OtherMessage ->
            % 他のメッセージ (wx イベントなど) は無視して待機を続ける
            await_close_message()
    end.


destroy() ->
    wx:destroy(),
    ok.
