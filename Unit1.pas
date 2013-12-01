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

  { TForm1 }
  TForm1 = class(TForm)
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
  Form1: TForm1;
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

procedure TForm1.SaveConfig(Filename: string);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(Filename);
  Ini.WriteString('ADMIN', 'IP', Host.Text);
  Ini.WriteString('ADMIN', 'Port', Port.Text);
  Ini.WriteString('ADMIN', 'Refresh', iif(Auto.Checked, '1', '0'));

  Ini.Free;
end;

procedure TForm1.LoadConfig(Filename: string);
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

  Ini.Free;
  Conf.Free;
end;

procedure TForm1.ConnectClick(Sender: TObject);
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

procedure TForm1.CmdKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

procedure TForm1.ClientConnected(Sender: TObject);
begin
  try
    Client.IOHandler.WriteLn(Pass.Text);
  except
  end;
  ServerConnectionStateChanged(False);
end;

procedure TForm1.ServerCredentialsEditKeyPress(Sender: TObject; var Key: char);
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

procedure TForm1.ServerConnectionStateChanged(NewState: Boolean);
begin
  Connect.Caption := iif(NewState, 'Disconnect', 'Connect');
  Refresh.Enabled := not NewState;
  Shutdown.Enabled := not NewState;
  Host.Enabled := NewState;
  Port.Enabled := NewState;
  Pass.Enabled := NewState;
end;

procedure TForm1.TimerTimer(Sender: TObject);
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

  Msg := Client.IOHandler.ReadLn('', TIMEOUT);

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

procedure TForm1.ClientDisconnected(Sender: TObject);
begin
  Memo.Lines.Add('Admin disconnected');
end;

procedure TForm1.CmdKeyPress(Sender: TObject; var Key: Char);
const
  ENTER = #13;
  NONE = #0;
begin
  if Key = ENTER then
  begin
    try
      if Client.Connected then
        Client.IOHandler.WriteLn(Cmd.Text)
      else
        Memo.Lines.Add(Cmd.Text);
      LastCmd := Cmd.Text;
      Cmd.Text := '';
    except
    end;
    Key := NONE;  // disable beep sound
  end;
end;

procedure TForm1.RefreshClick(Sender: TObject);
begin
  try
    if Client.Connected then
      Client.IOHandler.WriteLn('REFRESH');
  except
  end;
end;

procedure TForm1.DoCurrentPlayerAction(Name: string);
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

procedure TForm1.Kick1Click(Sender: TObject);
begin
  DoCurrentPlayerAction('/kick');
end;

procedure TForm1.Admin1Click(Sender: TObject);
begin
  DoCurrentPlayerAction('/adm');
end;

procedure TForm1.Ban1Click(Sender: TObject);
begin
  DoCurrentPlayerAction('/ban');
end;

procedure TForm1.RefreshTimerTimer(Sender: TObject);
begin
  if (Auto.Checked) and (Form1.WindowState <> wsMinimized) then
    RefreshClick(nil);
end;

procedure TForm1.ShutdownClick(Sender: TObject);
begin
  try
    Client.IOHandler.WriteLn('SHUTDOWN');
  except
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // dynamically created to avoid design time packages
  Client := TIdTCPClient.Create(nil);
  Client.OnConnected := ClientConnected;
  Client.OnDisconnected := ClientDisconnected;
  Client.Port := DEFAULT_PORT;
  Client.ReadTimeout := -1;

  LoadConfig(ExtractFilePath(Application.ExeName) + CONFIG_FILE);
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  try
    if Client.Connected then
      Client.Disconnect;
  except
  end;
  SaveConfig(ExtractFilePath(Application.ExeName) + CONFIG_FILE);
end;

procedure TForm1.ServerCredentialsEditChange(Sender: TObject);
begin
  if Sender is TEdit then
    TEdit(Sender).Color := COLOR_OK;
end;

end.
