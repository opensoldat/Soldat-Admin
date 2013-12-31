{************************************************************}
{                                                            }
{       Net Unit for Soldat Admin                            }
{                                                            }
{       Copyright (c) 2013 Gregor A. Cieslak                 }
{                                                            }
{       free to distribute and modify                        }
{************************************************************}

unit Net;

interface

const
  PLAYERNAME_CHARS = 24;
  MAX_PLAYERS = 32;
  MAX_TEAMS = 4;
  MAPNAME_CHARS = 16;

type
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

implementation

end.
