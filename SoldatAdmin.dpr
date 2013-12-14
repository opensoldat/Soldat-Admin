{************************************************************}
{                                                            }
{       Soldat Admin                                         }
{                                                            }
{       Copyright (c) 2003-2004 Michal Marcinkowski          }
{                                                            }
{       free to distribute and modify                        }
{************************************************************}

program SoldatAdmin;

{$IFDEF FPC}{$MODE Delphi}{$ENDIF}

uses
  {$IFDEF FPC}Interfaces,{$ENDIF}
  Forms,
  Unit1 in 'Unit1.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Soldat Admin';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
