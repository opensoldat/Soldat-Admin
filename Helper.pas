{************************************************************}
{                                                            }
{       Helper Unit for Soldat Admin                         }
{                                                            }
{       Copyright (c) 2013 Gregor A. Cieslak                 }
{                                                            }
{       free to distribute and modify                        }
{************************************************************}

unit Helper;

interface

function iif(Condition: Boolean; TrueCase, FalseCase: Variant): Variant;

implementation

function iif(Condition: Boolean; TrueCase, FalseCase: Variant): Variant;
begin
  if Condition then
    Result := TrueCase
  else
    Result := FalseCase;
end;

end.
