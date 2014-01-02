{************************************************************}
{                                                            }
{       Main Unit for Soldat Admin                           }
{                                                            }
{       Copyright (c) 2003-2004 Michal Marcinkowski          }
{       Copyright (c) 2012-2013 Gregor A. Cieslak            }
{                                                            }
{       free to distribute and modify                        }
{************************************************************}

unit Unit1;

{$IFDEF FPC}{$MODE Delphi}{$ENDIF}

interface

uses
  // general
  {$IFNDEF FPC}Windows,{$ELSE}LCLIntf, LCLType, LMessages, variants,{$ENDIF}
  SysUtils, Classes, Controls, Forms, StdCtrls, ExtCtrls, ComCtrls, Menus,
  Graphics,
  // network
  IdTCPClient, IdGlobal, Net,
  // settins
  Config;

const
  PLAYERNAME_CHARS = 24;
  MAX_PLAYERS = 32;
  MAX_TEAMS = 4;
  TEAM_SPECTATOR = 5;
  MAPNAME_CHARS = 16;
  DEFAULT_PORT = 23073;
  CONFIG_FILE = 'admin.ini';
  COLOR_OK = {$IFDEF FPC}clDefault{$ELSE}clWindow{$ENDIF};

type

  { TMainForm }
  TMainForm = class(TForm)
    ServerOutputMemo: TMemo;
    Timer: TTimer;
    Cmd: TEdit;
    ServerCommandLabel: TLabel;
    GameGroup: TGroupBox;
    RemoteServerGroup: TGroupBox;
    IpLabel: TLabel;
    Host: TEdit;
    Port: TEdit;
    PortLabel: TLabel;
    Pass: TEdit;
    PasswordLabel: TLabel;
    Connect: TButton;
    PlayerList: TListView;
    MapName: TLabel;
    TeamScoresGroup: TGroupBox;
    Team1Label: TLabel;
    Team2Label: TLabel;
    Team3Label: TLabel;
    Team4Label: TLabel;
    Refresh: TButton;
    AutoRefreshCheckBox: TCheckBox;
    PlayerPopup: TPopupMenu;
    KickMenuItem: TMenuItem;
    BanMenuItem: TMenuItem;
    AdminMenuItem: TMenuItem;
    Time: TLabel;
    Limit: TLabel;
    GameMode: TLabel;
    RefreshTimer: TTimer;
    Shutdown: TButton;
    procedure CmdKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ConnectClick(Sender: TObject);
    procedure ClientConnected(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ServerCredentialsEditKeyPress(Sender: TObject; var Key: char);
    procedure TimerTimer(Sender: TObject);
    procedure ClientDisconnected(Sender: TObject);
    procedure CmdKeyPress(Sender: TObject; var Key: Char);
    procedure RefreshClick(Sender: TObject);
    procedure KickMenuItemClick(Sender: TObject);
    procedure AdminMenuItemClick(Sender: TObject);
    procedure BanMenuItemClick(Sender: TObject);
    procedure RefreshTimerTimer(Sender: TObject);
    procedure ShutdownClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SaveConfig(Filename: string);
    procedure LoadConfig(Filename: string);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure DoCurrentPlayerAction(Name: string);
    procedure ServerCredentialsEditChange(Sender: TObject);
    procedure ServerConnectionStateChanged(NewState: Boolean);
    procedure PlayerListMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;
  RefreshMsg: TMsg_Refresh;
  LastCmd: TStringList;
  LastCmdIndex: Integer;
  Client: TIdTCPClient;

implementation

{$IFNDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TMainForm.SaveConfig(Filename: string);
begin
  Config.SaveConfig(Filename);
end;

procedure TMainForm.LoadConfig(Filename: string);
begin
  Config.LoadConfig(Filename);
end;

procedure TMainForm.ConnectClick(Sender: TObject);
var
  PortAsNumber: Integer;
  InvalidInput, IsConnected: Boolean;
const
  COLOR_ERROR = TColor($7F7FFF);
begin
  try
    IsConnected := Client.Connected;
  except
    IsConnected := False;
    try
      Client.Disconnect;
    except
    end;
  end;

  if not IsConnected then
  begin
    Connect.Caption := 'Connecting...';

    InvalidInput := False;

    // clear colors from last connection attempt
    Host.Color := COLOR_OK;
    Port.Color := COLOR_OK;
    Pass.Color := COLOR_OK;

    if Host.Text = '' then
    begin
      Host.Color := COLOR_ERROR;
      InvalidInput := True;
    end;

    PortAsNumber := StrToIntDef(Port.Text, -1);
    if PortAsNumber = -1 then
    begin
      Port.Color := COLOR_ERROR;
      InvalidInput := True;
    end;

    if Pass.Text = '' then
    begin
      Pass.Color := COLOR_ERROR;
      InvalidInput := True;
    end;

    // avoid edit field update delay
    Application.HandleMessage;

    if InvalidInput then
    begin
      ServerOutputMemo.Lines.Add('Invalid input.');
      Connect.Enabled := True;
      Exit;
    end;

    Client.Host := Host.Text;
    Client.Port := PortAsNumber;

    try
      Client.Connect;
    except
      Connect.Caption := 'Connect';
      ServerOutputMemo.Lines.Add('Connection failed');
    end;
  end
  else
  begin
    try
      Client.Disconnect;
    except
    end;
    ServerConnectionStateChanged(True);
  end;
end;

procedure TMainForm.CmdKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
const
  NONE = 0;
begin
  if Key = VK_UP then
  begin
    Key := NONE;  // disable default UP key action

    if LastCmdIndex < LastCmd.Count - 1 then
    begin
      if LastCmd[LastCmdIndex] <> Cmd.Text then
      begin
        LastCmd.Insert(1, Cmd.Text);
        Inc(LastCmdIndex);
      end;

      Inc(LastCmdIndex);
      Cmd.Text := LastCmd[LastCmdIndex];
      Cmd.SelStart := Length(Cmd.Text);
      Cmd.SelLength := 0;
    end;
  end
  else if Key = VK_DOWN then
  begin
    Key := NONE;  // disable default UP key action

    if LastCmdIndex > 0 then
    begin
      Dec(LastCmdIndex);
      Cmd.Text := LastCmd[LastCmdIndex];
      Cmd.SelStart := Length(Cmd.Text);
      Cmd.SelLength := 0;
    end;
  end;
end;

procedure TMainForm.ClientConnected(Sender: TObject);
begin
  try
    Client.IOHandler.WriteLn(Pass.Text, IndyTextEncoding_8Bit);
  except
  end;
  ServerConnectionStateChanged(False);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  {$IFDEF FPC}
  LoadConfig(ExtractFilePath(Application.ExeName) + CONFIG_FILE);
  {$ENDIF}
end;

procedure TMainForm.ServerCredentialsEditKeyPress(Sender: TObject; var Key: char);
const
  ENTER = #13;
  NONE = #0;
begin
  if Key = ENTER then
  begin
    ConnectClick(nil);
    Key := NONE;
  end;
end;

procedure TMainForm.ServerConnectionStateChanged(NewState: Boolean);
begin
  Connect.Caption := iif(NewState, 'Connect', 'Disconnect');
  Refresh.Enabled := not NewState;
  Shutdown.Enabled := not NewState;
  Host.Enabled := NewState;
  Port.Enabled := NewState;
  Pass.Enabled := NewState;
  Cmd.Enabled := not NewState;
end;

procedure TMainForm.TimerTimer(Sender: TObject);
var
  Msg: string;
  ListItem: TListItem;
  I: Integer;
  Buffer: TIdBytes;
  IsConnected: Boolean;
const
  TIMEOUT = 5;
  REFRESH_TIMEOUT = 2000;
begin
  try
    IsConnected := Client.Connected;
  except
    IsConnected := False;
    try
      Client.Disconnect;
    except
    end;
  end;

  if not IsConnected then
  begin
    if Refresh.Enabled then
      ServerConnectionStateChanged(True);
    Exit;
  end;

  Msg := Client.IOHandler.ReadLn('', TIMEOUT, -1, IndyTextEncoding_8Bit);

  if Msg <> '' then
  begin
    if Msg = 'REFRESH' then
    begin
      Client.ReadTimeout := REFRESH_TIMEOUT;
      Client.IOHandler.ReadBytes(Buffer, SizeOf(RefreshMsg), False);
      BytesToRaw(Buffer, RefreshMsg, SizeOf(RefreshMsg));

      PlayerList.Clear;
      for I := 1 to MAX_PLAYERS do
        if RefreshMsg.Team[I] < TEAM_SPECTATOR then
        begin
          ListItem := PlayerList.Items.Add;
          ListItem.Caption := RefreshMsg.Name[I];
          ListItem.SubItems.Add(IntToStr(RefreshMsg.Kills[I]));
          ListItem.SubItems.Add(IntToStr(RefreshMsg.Deaths[I]));
          ListItem.SubItems.Add(IntToStr(RefreshMsg.Ping[I]));
          ListItem.SubItems.Add(IntToStr(RefreshMsg.Team[I]));
          ListItem.SubItems.Add(IntToStr(RefreshMsg.IP[I][1]) + '.' +
            IntToStr(RefreshMsg.IP[I][2]) + '.' +
            IntToStr(RefreshMsg.IP[I][3]) + '.' +
            IntToStr(RefreshMsg.IP[I][4]));
          ListItem.SubItems.Add(IntToStr(RefreshMsg.Number[I]));
        end;

      MapName.Caption := 'Map: ' + RefreshMsg.MapName;
      Team1Label.Caption := 'Alpha: ' + IntToStr(RefreshMsg.TeamScore[1]);
      Team2Label.Caption := 'Bravo: ' + IntToStr(RefreshMsg.TeamScore[2]);
      Team3Label.Caption := 'Charlie: ' + IntToStr(RefreshMsg.TeamScore[3]);
      Team4Label.Caption := 'Delta: ' + IntToStr(RefreshMsg.TeamScore[4]);
      Time.Caption := 'Time: ' + IntToStr((RefreshMsg.CurrentTime div 3600)) +
        '/' + IntToStr((RefreshMsg.TimeLimit div 3600));
      Limit.Caption := 'Score Limit: ' + IntToStr(RefreshMsg.KillLimit);
      case RefreshMsg.GameStyle of
        0: GameMode.Caption := 'Game Mode: DM';
        1: GameMode.Caption := 'Game Mode: PM';
        2: GameMode.Caption := 'Game Mode: TM';
        3: GameMode.Caption := 'Game Mode: CTF';
        4: GameMode.Caption := 'Game Mode: RM';
        5: GameMode.Caption := 'Game Mode: INF';
      end;

      Exit;
    end else if Copy(Msg, 0, 13) = '/clientlist (' then
    begin
      try
        Client.IOHandler.WriteLn('[ª] User: ' + MainForm.Caption,
          IndyTextEncoding_8Bit);
      except
      end;
    end;

    ServerOutputMemo.Lines.Add(Msg);
    if (Msg = 'Invalid server password. Cannot login.') or
      (Msg = 'Invalid password.') then
    begin
      ServerConnectionStateChanged(True);
      try
        Client.Disconnect;
      except
      end;
      Exit;
    end;
  end;
end;

procedure TMainForm.ClientDisconnected(Sender: TObject);
begin
  ServerOutputMemo.Lines.Add('Admin disconnected');
end;

procedure TMainForm.CmdKeyPress(Sender: TObject; var Key: Char);
const
  ENTER = #13;
  NONE = #0;
begin
  if Key = ENTER then
  begin
    try
      if Client.Connected then
      begin
        Client.IOHandler.WriteLn(Cmd.Text, IndyTextEncoding_8Bit);
        LastCmd.Insert(1, Cmd.Text);
        LastCmdIndex := 0;
        Cmd.Text := '';
      end;
    except
    end;
    Key := NONE;  // disable beep sound
  end;
end;

procedure TMainForm.RefreshClick(Sender: TObject);
begin
  try
    if Client.Connected then
      Client.IOHandler.WriteLn('REFRESH', IndyTextEncoding_8Bit);
  except
  end;
end;

procedure TMainForm.DoCurrentPlayerAction(Name: string);
var
  I: Integer;
  S: string;
  Ch: Char;
const
  ENTER = #13;
begin
  if PlayerList.Items.Count = 0 then
    Exit;
  I := PlayerList.ItemIndex;
  if (I < 0) or (I > (PlayerList.Items.Count - 1)) then
    Exit;

  S := PlayerList.Items[I].Caption;

  Cmd.Text := Name + ' ' + S;
  Ch := ENTER;
  CmdKeyPress(nil, Ch);
end;

procedure TMainForm.KickMenuItemClick(Sender: TObject);
begin
  DoCurrentPlayerAction('/kick');
end;

procedure TMainForm.AdminMenuItemClick(Sender: TObject);
begin
  DoCurrentPlayerAction('/adm');
end;

procedure TMainForm.BanMenuItemClick(Sender: TObject);
begin
  DoCurrentPlayerAction('/ban');
end;

procedure TMainForm.RefreshTimerTimer(Sender: TObject);
begin
  if (AutoRefreshCheckBox.Checked) and (MainForm.WindowState <> wsMinimized) then
    RefreshClick(nil);
end;

procedure TMainForm.ShutdownClick(Sender: TObject);
begin
  try
    Client.IOHandler.WriteLn('SHUTDOWN', IndyTextEncoding_8Bit);
  except
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // dynamically created to avoid design time packages
  Client := TIdTCPClient.Create(nil);
  Client.OnConnected := ClientConnected;
  Client.OnDisconnected := ClientDisconnected;
  Client.Port := DEFAULT_PORT;
  Client.ReadTimeout := -1;

  LastCmd := TStringList.Create;
  LastCmd.Insert(0, '');
  LastCmdIndex := 0;

  {$IFNDEF FPC}
  LoadConfig(ExtractFilePath(Application.ExeName) + CONFIG_FILE);
  {$ENDIF}
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  try
    if Client.Connected then
      Client.Disconnect;
  except
  end;
  SaveConfig(ExtractFilePath(Application.ExeName) + CONFIG_FILE);
end;

procedure TMainForm.ServerCredentialsEditChange(Sender: TObject);
begin
  if Sender is TEdit then
    TEdit(Sender).Color := COLOR_OK;
end;

procedure TMainForm.PlayerListMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  clickPos: TPoint;
  I: Integer;
begin
  if Button = mbRight then
  begin
    if PlayerList.Items.Count = 0 then
      Exit;
    I := PlayerList.ItemIndex;
    if (I < 0) or (I > (PlayerList.Items.Count - 1)) then
      Exit;

    GetCursorPos(clickPos);
    PlayerPopup.Popup(clickPos.X, clickPos.Y);
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  LastCmd.Free;
end;

end.
