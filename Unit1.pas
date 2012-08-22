 {
       Soldat Admin
        for SOLDAT

 Copyright © 2003-04 Michal Marcinkowski

   free to distribute and modify
 }
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, IdBaseComponent, IdComponent, IdTCPServer,
  IdTCPConnection, IdTCPClient, ExtCtrls, ComCtrls, Menus, IniFiles;

const
 PLAYERNAME_CHARS = 24;
   
type
  TForm1 = class(TForm)
    Client: TIdTCPClient;
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
    procedure SaveConfig(filename : string);
    procedure LoadConfig(filename : string);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


 TMsg_Refresh = packed record
  Name : array[1..32] of string[PLAYERNAME_CHARS];
  Team : array[1..32] of byte;
  Kills : array[1..32] of word;
  Deaths : array[1..32] of word;
  Ping : array[1..32] of byte;
  Number : array[1..32] of byte;
  IP : array[1..32,1..4] of byte;
  TeamScore : array[1..4] of word;
  MapName : string[16];
  TimeLimit, CurrentTime : integer;
  KillLimit : word;
  GameStyle : byte;
 end;


var
  Form1: TForm1;

  RefreshMsg : TMsg_Refresh;

  LastCmd : string = '';


implementation

{$R *.dfm}


procedure TForm1.SaveConfig(filename : string);
var
 ini:Tinifile;
 conf:Tstringlist;
 s : string;
 i : longint;
begin
conf:=Tstringlist.create;
ini:=Tinifile.Create(filename);
 Ini.WriteString('ADMIN','IP',Host.Text);
 Ini.WriteString('ADMIN','Port',Port.Text);
ini.free;
conf.free;
end;

procedure TForm1.LoadConfig(filename : string);
var
 ini:Tmeminifile;
 conf:Tstringlist;
 s : string;
 i : longint;
begin
conf:=Tstringlist.create;
ini:=Tmeminifile.Create(filename);
if not assigned(ini) then exit;

 ini.ReadSectionValues('ADMIN',conf);
 Host.Text:= conf.values['IP'];
 Port.Text:= conf.values['Port'];

ini.free;
conf.free;
end;



procedure TForm1.ConnectClick(Sender: TObject);
begin
if not Client.Connected then
begin
 Client.Host := Host.Text;
 Client.Port:= strtoint(Port.Text);
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
 Connect.Caption:= 'Connect';
 Refresh.Enabled:= false;
 Shutdown.Enabled:= false;
 end;
end;

procedure TForm1.ClientConnected(Sender: TObject);
begin
 {try
 Client.WriteLn(Pass.Text);
 except
 end;     }
 Connect.Caption:= 'Disconnect';

 Refresh.Enabled:= true;
 Shutdown.Enabled:= true;
end;

procedure TForm1.TimerTimer(Sender: TObject);
var
  Com,
  Msg : String;
  ListItem : TListItem;
  i : integer;
begin
  if not Client.Connected then
    exit;

  Msg := Client.ReadLn('', 5);

  if Msg <> '' then
  begin
   //////////REFRESH//////////////////////////

    if Msg='REFRESH' then
    begin
    // Memo.Lines.Add('Receiving server state...');
     Client.ReadBuffer(RefreshMsg, sizeof(RefreshMsg));

     for i:= 1 to 32 do
     if RefreshMsg.Team[i]<5 then
     begin
      ListItem:= PlayerList.Items.Add;
      ListItem.Caption:= RefreshMsg.Name[i];
      ListItem.SubItems.Add(inttostr(RefreshMsg.Kills[i]));
      ListItem.SubItems.Add(inttostr(RefreshMsg.Deaths[i]));
      ListItem.SubItems.Add(inttostr(RefreshMsg.Ping[i]));
      ListItem.SubItems.Add(inttostr(RefreshMsg.Team[i]));
      ListItem.SubItems.Add(inttostr(RefreshMsg.IP[i][1])+'.'+inttostr(RefreshMsg.IP[i][2])+'.'+inttostr(RefreshMsg.IP[i][3])+'.'+inttostr(RefreshMsg.IP[i][4]) );
      ListItem.SubItems.Add(inttostr(RefreshMsg.Number[i]));
     end;

      MapName.Caption:= 'Map: '+RefreshMsg.MapName;
      Team1.Caption:= 'Alpha: ' + inttostr(RefreshMsg.TeamScore[1]);
      Team2.Caption:= 'Bravo: ' + inttostr(RefreshMsg.TeamScore[2]);
      Team3.Caption:= 'Charlie: ' + inttostr(RefreshMsg.TeamScore[3]);
      Team4.Caption:= 'Delta: ' + inttostr(RefreshMsg.TeamScore[4]);
      Time.Caption:= 'Time: '+inttostr((RefreshMsg.CurrentTime div 3600)) +'/'+ inttostr((RefreshMsg.TimeLimit div 3600)) ;
      Limit.Caption:= 'Score Limit: '+inttostr(RefreshMsg.KillLimit);
      case RefreshMsg.GameStyle of
       0 : GameMode.Caption:= 'Game Mode: DM';
       1 : GameMode.Caption:= 'Game Mode: PM';
       2 : GameMode.Caption:= 'Game Mode: TM';
       3 : GameMode.Caption:= 'Game Mode: CTF';
       4 : GameMode.Caption:= 'Game Mode: RM';
       5 : GameMode.Caption:= 'Game Mode: INF';
      end;

   //  Memo.Lines.Add('Server state refreshed');
     exit;
    end;

   //////////REFRESH//////////////////////////

   Memo.Lines.Add(Msg);
   if Msg='Invalid server password. Cannot login.' then
   begin
     Connect.Caption:= 'Connect';
     Refresh.Enabled:= false;
     Shutdown.Enabled:= false;
    try
    Client.Disconnect;
    except
    end;
    exit;
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
 if Key=#13 then
 begin
 try
  if Client.Connected then
  Client.WriteLn(Cmd.Text) else
  Memo.Lines.Add(Cmd.Text);
  LastCmd:= Cmd.Text;
  Cmd.Text:= '';
 except
 end;
 end;

 if Key=#8 then
 begin
  if Cmd.Text='' then
  Cmd.Text:= LastCmd;
 end;
end;

procedure TForm1.ExitButtonClick(Sender: TObject);
begin
 try
 if Client.Connected then
  Client.Disconnect;
 except
 end;
 SaveConfig(ExtractFilePath(Application.ExeName)+'admin.ini');
 Close;
end;

procedure TForm1.RefreshClick(Sender: TObject);
begin
 PlayerList.Clear;

 try
  Client.WriteLn('REFRESH')
 except
 end;
end;

procedure TForm1.PlayerListMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
 m : tpoint;
begin
 if Button = mbRight then
 begin
  GetCursorPos(m);
  PlayerList.PopupMenu.Popup(m.x, m.y);
 end;
end;

procedure TForm1.Kick1Click(Sender: TObject);
var
 i : integer;
 S : string;
 ch : char;
begin
 if PlayerList.Items.Count=0 then exit;
 i:= PlayerList.Itemindex;
 if (i<0)or(i>(PlayerList.Items.Count-1)) then exit;

 S:= PlayerList.Items[i].Caption;

 Cmd.Text:= '/kick ' + S;
 ch:= #13;
 CmdKeyPress(nil, ch);
end;

procedure TForm1.Admin1Click(Sender: TObject);
var
 i : integer;
 S : string;
 ch : char;
begin
 if PlayerList.Items.Count=0 then exit;
 i:= PlayerList.Itemindex;
 if (i<0)or(i>(PlayerList.Items.Count-1)) then exit;

 S:= PlayerList.Items[i].Caption;

 Cmd.Text:= '/adm ' + S;
 ch:= #13;
 CmdKeyPress(nil, ch);
end;

procedure TForm1.Ban1Click(Sender: TObject);
var
 i : integer;
 S : string;
 ch : char;
begin
 if PlayerList.Items.Count=0 then exit;
 i:= PlayerList.Itemindex;
 if (i<0)or(i>(PlayerList.Items.Count-1)) then exit;

 S:= PlayerList.Items[i].Caption;

 Cmd.Text:= '/ban ' + S;
 ch:= #13;
 CmdKeyPress(nil, ch);
end;

procedure TForm1.RefreshTimerTimer(Sender: TObject);
begin
 if (Auto.Checked)and(Form1.WindowState<>wsMinimized) then RefreshClick(nil);
end;

procedure TForm1.ShutdownClick(Sender: TObject);
begin
 try
  Client.WriteLn('SHUTDOWN')
 except
 end; 
end;


procedure TForm1.FormCreate(Sender: TObject);
begin
 LoadConfig(ExtractFilePath(Application.ExeName)+'admin.ini');
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 SaveConfig(ExtractFilePath(Application.ExeName)+'admin.ini');
end;

end.
