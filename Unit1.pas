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
  IdTCPClient, IdGlobal,
  // other
  IniFiles;

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
    Memo: TMemo;
    Timer: TTimer;
    Cmd: TEdit;
    Label1: TLabel;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label3: TLabel;
    Host: TEdit;
    Port: TEdit;
    Label4: TLabel;
    Pass: TEdit;
    Label2: TLabel;
    Connect: TButton;
    PlayerList: TListView;
    MapName: TLabel;
    GroupBox3: TGroupBox;
    team1: TLabel;
    team2: TLabel;
    team3: TLabel;
    team4: TLabel;
    Refresh: TButton;
    Auto: TCheckBox;
    PlayerPopup: TPopupMenu;
    Kick1: TMenuItem;
    Ban1: TMenuItem;
    Admin1: TMenuItem;
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
    procedure Kick1Click(Sender: TObject);
    procedure Admin1Click(Sender: TObject);
    procedure Ban1Click(Sender: TObject);
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
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  // binary packets
  TMsg_Refresh = packed record
    Name: array[1..MAX_PLAYERS] of string[PLAYERNAME_CHARS];
    Team: array[1..MAX_PLAYERS] of Byte;
    Kills: array[1..MAX_PLAYERS] of Word;
    Deaths: array[1..MAX_PLAYERS] of Word;
    Ping: array[1..MAX_PLAYERS] of Byte;
    Number: array[1..MAX_PLAYERS] of Byte;
    IP: array[1..MAX_PLAYERS, 1..4] of Byte;
    TeamScore: array[1..MAX_TEAMS] of Word;
    MapName: string[MAPNAME_CHARS];
    TimeLimit, CurrentTime: Integer;
    KillLimit: Word;
    GameStyle: Byte;
  end;


var
  MainForm: TMainForm;
  RefreshMsg: TMsg_Refresh;
  LastCmd: string = '';
  Client: TIdTCPClient;

implementation

{$IFNDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

function iif(Condition: Boolean; TrueCase, FalseCase: Variant): Variant;
begin
  if Condition then
    Result := TrueCase
  else
    Result := FalseCase;
end;

procedure TMainForm.SaveConfig(Filename: string);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(Filename);
  Ini.WriteString('ADMIN', 'IP', Host.Text);
  Ini.WriteString('ADMIN', 'Port', Port.Text);
  Ini.WriteString('ADMIN', 'Refresh', iif(Auto.Checked, '1', '0'));

  Ini.WriteString('WINDOW', 'Maximized', iif(MainForm.WindowState = wsMaximized, '1', '0'));
  if MainForm.WindowState <> wsMaximized then
  begin
    Ini.WriteString('WINDOW', 'X', IntToStr(MainForm.Left));
    Ini.WriteString('WINDOW', 'Y', IntToStr(MainForm.Top));
    Ini.WriteString('WINDOW', 'Width', IntToStr(MainForm.Width));
    Ini.WriteString('WINDOW', 'Height', IntToStr(MainForm.Height));
  end;

  Ini.Free;
end;

procedure TMainForm.LoadConfig(Filename: string);
var
  Ini: TMemIniFile;
  Conf: TStringList;
begin
  Conf := TStringList.Create;
  Ini := TMemIniFile.Create(Filename);
  if not Assigned(Ini) then
  begin
    Conf.Free;
    Exit;
  end;

  Ini.ReadSectionValues('ADMIN', Conf);
  Host.Text := Conf.Values['IP'];
  Port.Text := Conf.Values['Port'];
  Auto.Checked := Conf.Values['Refresh'] = '1';
  Conf.Clear;

  Ini.ReadSectionValues('WINDOW', Conf);
  if Conf.Values['Maximized'] = '1' then
  begin
    MainForm.WindowState := wsMaximized;
  end else
  begin
    try
      MainForm.Left := StrToInt(Conf.Values['X']);
      MainForm.Top := StrToInt(Conf.Values['Y']);
      MainForm.Position := poDesigned;
    except
    end;

    MainForm.Width := StrToIntDef(Conf.Values['Width'], MainForm.Constraints.MinWidth);
    MainForm.Height := StrToIntDef(Conf.Values['Height'], MainForm.Constraints.MinHeight);
  end;

  Ini.Free;
  Conf.Free;
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
      Memo.Lines.Add('Invalid input.');
      Connect.Enabled := True;
      Exit;
    end;

    Client.Host := Host.Text;
    Client.Port := PortAsNumber;

    try
      Client.Connect;
    except
      Connect.Caption := 'Connect';
      Memo.Lines.Add('Connection failed');
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
    Cmd.Text := LastCmd;
    Cmd.SelStart := Length(Cmd.Text);
    Cmd.SelLength := 0;
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
  {$IFDEF FPC}LoadConfig(ExtractFilePath(Application.ExeName) + CONFIG_FILE);{$ENDIF}
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
      Team1.Caption := 'Alpha: ' + IntToStr(RefreshMsg.TeamScore[1]);
      Team2.Caption := 'Bravo: ' + IntToStr(RefreshMsg.TeamScore[2]);
      Team3.Caption := 'Charlie: ' + IntToStr(RefreshMsg.TeamScore[3]);
      Team4.Caption := 'Delta: ' + IntToStr(RefreshMsg.TeamScore[4]);
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
        Client.IOHandler.WriteLn('[ª] User: ' + MainForm.Caption, IndyTextEncoding_8Bit);
      except
      end;
    end;

    Memo.Lines.Add(Msg);
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
  Memo.Lines.Add('Admin disconnected');
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
        LastCmd := Cmd.Text;
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

procedure TMainForm.Kick1Click(Sender: TObject);
begin
  DoCurrentPlayerAction('/kick');
end;

procedure TMainForm.Admin1Click(Sender: TObject);
begin
  DoCurrentPlayerAction('/adm');
end;

procedure TMainForm.Ban1Click(Sender: TObject);
begin
  DoCurrentPlayerAction('/ban');
end;

procedure TMainForm.RefreshTimerTimer(Sender: TObject);
begin
  if (Auto.Checked) and (MainForm.WindowState <> wsMinimized) then
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

  {$IFNDEF FPC}LoadConfig(ExtractFilePath(Application.ExeName) + CONFIG_FILE);{$ENDIF}
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

end.
