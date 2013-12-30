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

procedure LoadConfig(Filename: string);
procedure SaveConfig(Filename: string);

implementation

uses
  SysUtils, Classes, Forms, IniFiles, Unit1, Helper;

procedure SaveConfig(Filename: string);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(Filename);
  Ini.WriteString('ADMIN', 'IP', MainForm.Host.Text);
  Ini.WriteString('ADMIN', 'Port', MainForm.Port.Text);
  Ini.WriteString('ADMIN', 'Refresh', iif(MainForm.AutoRefreshCheckBox.Checked, '1', '0'));
  Ini.WriteString('ADMIN', 'Password', MainForm.Pass.Text);

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

procedure LoadConfig(Filename: string);
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
  MainForm.Host.Text := Conf.Values['IP'];
  MainForm.Port.Text := Conf.Values['Port'];
  MainForm.AutoRefreshCheckBox.Checked := Conf.Values['Refresh'] = '1';
  MainForm.Pass.Text := Conf.Values['Password'];
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

end.
