{************************************************************}
{                                                            }
{       Main Unit for Soldat Admin                           }
{                                                            }
{       Copyright (c) 2003-2004 Michal Marcinkowski          }
{                                                            }
{       free to distribute and modify                        }
{************************************************************}

unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, Menus,
  IdBaseComponent, IdComponent, IdTCPServer, IdTCPConnection, IdTCPClient,
  IniFiles;

const
  PLAYERNAME_CHARS = 24;
   
type
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
    ExitButton: TButton;
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
    procedure ConnectClick(Sender: TObject);
    procedure ClientConnected(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure ClientDisconnected(Sender: TObject);
    procedure CmdKeyPress(Sender: TObject; var Key: Char);
    procedure ExitButtonClick(Sender: TObject);
    procedure RefreshClick(Sender: TObject);
    procedure PlayerListMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Kick1Click(Sender: TObject);
    procedure Admin1Click(Sender: TObject);
    procedure Ban1Click(Sender: TObject);
    procedure RefreshTimerTimer(Sender: TObject);
    procedure ShutdownClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SaveConfig(Filename: string);
    procedure LoadConfig(Filename: string);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


  TMsg_Refresh = packed record
    Name: array[1..32] of string[PLAYERNAME_CHARS];
    Team: array[1..32] of Byte;
    Kills: array[1..32] of Word;
    Deaths: array[1..32] of Word;
    Ping: array[1..32] of Byte;
    Number: array[1..32] of Byte;
    IP: array[1..32,1..4] of Byte;
    TeamScore: array[1..4] of Word;
    MapName: string[16];
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

{$R *.dfm}


procedure TForm1.SaveConfig(Filename: string);
var
  Ini: TIniFile;
  Conf: TStringList;
  S: string;
  I: Longint;
begin
  conf := TStringList.Create;
  Ini := TIniFile.Create(Filename);
  Ini.WriteString('ADMIN', 'IP', Host.Text);
  Ini.WriteString('ADMIN', 'Port', Port.Text);
  Ini.Free;
  Conf.Free;
end;

procedure TForm1.LoadConfig(Filename: string);
var
  Ini: TMemIniFile;
  Conf: TStringList;
  S: string;
  I: Longint;
begin
  Conf := TStringList.Create;
  Ini := TMemIniFile.Create(Filename);
  if not Assigned(Ini) then
    Exit;

  Ini.ReadSectionValues('ADMIN', Conf);
  Host.Text := Conf.Values['IP'];
  Port.Text := Conf.Values['Port'];

  Ini.Free;
  Conf.Free;
end;

procedure TForm1.ConnectClick(Sender: TObject);
begin
  if not Client.Connected then
  begin
    Client.Host := Host.Text;
    Client.Port := StrToInt(Port.Text);
    try
      Client.Connect;
    except
    end;
  end else
  begin
    try
      Client.Disconnect;
    except
    end;
    Connect.Caption := 'Connect';
    Refresh.Enabled := False;
    Shutdown.Enabled := False;
  end;
end;

procedure TForm1.ClientConnected(Sender: TObject);
begin
  try
    Client.WriteLn(Pass.Text);
  except
  end;
  Connect.Caption := 'Disconnect';

  Refresh.Enabled := True;
  Shutdown.Enabled := True;
end;

procedure TForm1.TimerTimer(Sender: TObject);
var
  Com, Msg: string;
  ListItem: TListItem;
  I: Integer;
begin
  if not Client.Connected then
    Exit;

  Msg := Client.ReadLn('', 5);

  if Msg <> '' then
  begin
    //////////REFRESH//////////////////////////
    if Msg = 'REFRESH' then
    begin
      // Memo.Lines.Add('Receiving server state...');
      Client.ReadTimeout := 2000;
      Client.ReadBuffer(RefreshMsg, SizeOf(RefreshMsg));

      for I := 1 to 32 do
      if RefreshMsg.Team[I] < 5 then
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
        '/' + IntToStr((RefreshMsg.TimeLimit div 3600)) ;
      Limit.Caption := 'Score Limit: ' + IntToStr(RefreshMsg.KillLimit);
      case RefreshMsg.GameStyle of
        0: GameMode.Caption := 'Game Mode: DM';
        1: GameMode.Caption := 'Game Mode: PM';
        2: GameMode.Caption := 'Game Mode: TM';
        3: GameMode.Caption := 'Game Mode: CTF';
        4: GameMode.Caption := 'Game Mode: RM';
        5: GameMode.Caption := 'Game Mode: INF';
      end;

     // Memo.Lines.Add('Server state refreshed');
     Exit;
    end;
    //////////REFRESH//////////////////////////

    Memo.Lines.Add(Msg);
    if Msg = 'Invalid server password. Cannot login.' then
    begin
      Connect.Caption := 'Connect';
      Refresh.Enabled := False;
      Shutdown.Enabled := False;
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
  try
    Client.WriteLn('Admin disconnected');
  except
  end;
end;

procedure TForm1.CmdKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    try
      if Client.Connected then
        Client.WriteLn(Cmd.Text)
      else
        Memo.Lines.Add(Cmd.Text);
      LastCmd := Cmd.Text;
      Cmd.Text := '';
    except
    end;
  end;

  if Key = #8 then
  begin
    if Cmd.Text = '' then
      Cmd.Text := LastCmd;
  end;
end;

procedure TForm1.ExitButtonClick(Sender: TObject);
begin
  try
    if Client.Connected then
      Client.Disconnect;
  except
  end;
  SaveConfig(ExtractFilePath(Application.ExeName) + 'admin.ini');
  Close;
end;

procedure TForm1.RefreshClick(Sender: TObject);
begin
  PlayerList.Clear;

  try
    Client.WriteLn('REFRESH');
  except
  end;
end;

procedure TForm1.PlayerListMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  M: TPoint;
begin
  if Button = mbRight then
  begin
    GetCursorPos(M);
    PlayerList.PopupMenu.Popup(M.x, M.y);
  end;
end;

procedure TForm1.Kick1Click(Sender: TObject);
var
  I: Integer;
  S: string;
  Ch: Char;
begin
  if PlayerList.Items.Count = 0 then
    Exit;
  I := PlayerList.Itemindex;
  if (I < 0) or (I > (PlayerList.Items.Count - 1)) then
    Exit;

  S := PlayerList.Items[I].Caption;

  Cmd.Text:= '/kick ' + S;
  Ch := #13;
  CmdKeyPress(nil, Ch);
end;

procedure TForm1.Admin1Click(Sender: TObject);
var
  I: Integer;
  S: string;
  Ch: Char;
begin
  if PlayerList.Items.Count = 0 then
    Exit;
  I := PlayerList.ItemIndex;
  if (I < 0) or (I > (PlayerList.Items.Count - 1)) then
    Exit;

  S := PlayerList.Items[I].Caption;

  Cmd.Text := '/adm ' + S;
  Ch := #13;
  CmdKeyPress(nil, Ch);
end;

procedure TForm1.Ban1Click(Sender: TObject);
var
  I: Integer;
  S: string;
  Ch: Char;
begin
  if PlayerList.Items.Count = 0 then
    Exit;
  I := PlayerList.ItemIndex;
  if (I < 0) or (I > (PlayerList.Items.Count - 1)) then
    Exit;

  S := PlayerList.Items[I].Caption;

  Cmd.Text:= '/ban ' + S;
  Ch := #13;
  CmdKeyPress(nil, Ch);
end;

procedure TForm1.RefreshTimerTimer(Sender: TObject);
begin
  if (Auto.Checked) and (Form1.WindowState <> wsMinimized) then
    RefreshClick(nil);
end;

procedure TForm1.ShutdownClick(Sender: TObject);
begin
  try
    Client.WriteLn('SHUTDOWN');
  except
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Client := TIdTCPClient.Create(nil);
  Client.OnConnected := ClientConnected;
  Client.OnDisconnected := ClientDisconnected;
  Client.BoundIP := '0';
  Client.Port := 23073;
  Client.ReadTimeout := -1;

  LoadConfig(ExtractFilePath(Application.ExeName) + 'admin.ini');
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveConfig(ExtractFilePath(Application.ExeName) + 'admin.ini');
end;

end.
