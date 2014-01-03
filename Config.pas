{************************************************************}
{                                                            }
{       Config Unit for Soldat Admin                         }
{                                                            }
{       Copyright (c) 2013 Gregor A. Cieslak                 }
{                                                            }
{       free to distribute and modify                        }
{************************************************************}

unit Config;

interface

type
  TConfig = class
  private
    fFilename: string;

    fHost: string;
    fPort: string;
    fPassword: string;

    fAutoRefresh: Boolean;

    fMaximized: Boolean;
    fX: Integer;
    fY: Integer;
    fWidth: Integer;
    fHeight: Integer;

    fDefaultPosition: Boolean;

  public
    constructor Create(Filename: string);

    procedure Load;
    procedure Save;

    property Host: string read fHost write fHost;
    property Port: string read fPort write fPort;

    property Password: string read fPassword write fPassword;

    property AutoRefresh: Boolean read fAutoRefresh write fAutoRefresh;

    property Maximized: Boolean read fMaximized write fMaximized;
    property X: Integer read fX write fX;
    property Y: Integer read fY write fY;
    property Width: Integer read fWidth write fWidth;
    property Height: Integer read fHeight write fHeight;

    property DefaultPosition: Boolean read fDefaultPosition;
  end;

implementation

uses
  SysUtils, Classes, IniFiles, Helper;

constructor TConfig.Create(Filename: string);
begin
  fFilename := Filename;
end;

procedure TConfig.Load;
var
  Ini: TMemIniFile;
  Conf: TStringList;
begin
  Conf := TStringList.Create;
  Ini := TMemIniFile.Create(fFilename);
  if not Assigned(Ini) then
  begin
    Conf.Free;
    Exit;
  end;

  Ini.ReadSectionValues('ADMIN', Conf);
  Host := Conf.Values['IP'];
  Port := Conf.Values['Port'];
  AutoRefresh := Conf.Values['Refresh'] = '1';
  Password := Conf.Values['Password'];
  Conf.Clear;

  Ini.ReadSectionValues('WINDOW', Conf);
  Maximized := Conf.Values['Maximized'] = '1';

  try
    X := StrToInt(Conf.Values['X']);
    Y := StrToInt(Conf.Values['Y']);
    fDefaultPosition := False;
  except
    X := -1;
    Y := -1;
    fDefaultPosition := True;
  end;

  Width := StrToIntDef(Conf.Values['Width'], -1);
  Height := StrToIntDef(Conf.Values['Height'], -1);

  Ini.Free;
  Conf.Free;
end;

procedure TConfig.Save;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(fFilename);
  Ini.WriteString('ADMIN', 'IP', Host);
  Ini.WriteString('ADMIN', 'Port', Port);
  Ini.WriteString('ADMIN', 'Refresh', iif(AutoRefresh, '1', '0'));
  Ini.WriteString('ADMIN', 'Password', Password);

  Ini.WriteString('WINDOW', 'Maximized', iif(Maximized, '1', '0'));
  if not Maximized then
  begin
    Ini.WriteString('WINDOW', 'X', IntToStr(X));
    Ini.WriteString('WINDOW', 'Y', IntToStr(Y));
    Ini.WriteString('WINDOW', 'Width', IntToStr(Width));
    Ini.WriteString('WINDOW', 'Height', IntToStr(Height));
  end;

  Ini.Free;
end;

end.
