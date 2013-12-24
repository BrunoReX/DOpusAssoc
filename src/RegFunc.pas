{
Copyright (c) 2013 Bruno Barbieri
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of the <organization> nor the
      names of its contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
}

unit RegFunc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Registry;

function RegKeyExists(root: HKEY; key: String): Boolean;
function GetRegKeyValue(root: HKEY; key: String; subKey: String = ''): String;
function GetRegKeyValues(root: HKEY; keys: TStringList; subKey: String = ''): TStringList;
function CreateRegKey(root: HKEY; key: String): Boolean;
function CreateRegValue(root: HKEY; key: String; subKey: String; value: String): Boolean;
function DeleteRegKey(root: HKEY; key: String): Boolean;
function DeleteRegValue(root: HKEY; key: String; subKey: String): Boolean;
procedure CreateRegValues(root: HKEY; keys: TStringList; subKey: String; value: String; checkCurrent: Boolean = false);

implementation

function RegKeyExists(root: HKEY; key: String): Boolean;
var
  Registry: TRegistry;
begin
  Registry := TRegistry.Create;
  try
    Registry.RootKey := root;
    if Registry.OpenKeyReadOnly('\'+key) then
      Result := True
    else
      Result := False;
  except
    Result := False;
  end;

  Registry.Free;
end;

function GetRegKeyValue(root: HKEY; key: String; subKey: String = ''): String;
var
  Registry: TRegistry;
begin
  Registry := TRegistry.Create;

  try
    Registry.RootKey := root;
    if Registry.OpenKeyReadOnly('\'+key) then
      Result := Registry.ReadString(subKey)
    else
      Result := '';
  except
    Result := '';
  end;

  Registry.Free;
end;

function GetRegKeyValues(root: HKEY; keys: TStringList; subKey: String = ''): TStringList;
var
  i: Integer;
  MAX_KEYS: Integer;
  results: TStringList;
begin
  MAX_KEYS := keys.Count;
  results := TStringList.Create;

  for i := 0 to MAX_KEYS-1 do
    begin
      results.Add(GetRegKeyValue(root, '\'+keys.ValueFromIndex[i], subKey));
    end;

  Result := results;
end;

function CreateRegKey(root: HKEY; key: String): Boolean;
var
  Registry: TRegistry;
begin
  Registry := TRegistry.Create;
  try
    Registry.RootKey := root;

    if Registry.CreateKey('\'+key) then
      Result := True
    else
      Result := False;
  except
    Result := False;
  end;

  Registry.Free;
end;

function CreateRegValue(root: HKEY; key: String; subKey: String; value: String): Boolean;
var
  Registry: TRegistry;
begin
  Registry := TRegistry.Create;
  try
    CreateRegKey(root, '\'+key);
    Registry.RootKey := root;

    if Registry.OpenKey('\'+key, True) then
     begin
       Registry.WriteString(subKey, value);
       Result := True;
     end
    else
      Result := False;
  except
    Result := False;
  end;

  Registry.Free;
end;

function DeleteRegKey(root: HKEY; key: String): Boolean;
var
  Registry: TRegistry;
begin
  Registry := TRegistry.Create;
  try
    Registry.RootKey := root;

    if Registry.DeleteKey('\'+key) then
      Result := True
    else
      Result := False;
  except
    Result := False;
  end;

  Registry.Free;
end;

function DeleteRegValue(root: HKEY; key: String; subKey: String): Boolean;
var
  Registry: TRegistry;
begin
  Registry := TRegistry.Create;
  try
    Registry.RootKey := root;

    if Registry.OpenKey('\'+key, True) then
     begin
       Registry.DeleteValue(subKey);
       Result := True;
     end
    else
      Result := False;
  except
    Result := False;
  end;

  Registry.Free;
end;

procedure CreateRegValues(root: HKEY; keys: TStringList; subKey: String; value: String; checkCurrent: Boolean = false);
var
  i: Integer;
  MAX_KEYS: Integer;
begin
  MAX_KEYS := keys.Count;

  for i := 0 to MAX_KEYS-1 do
    begin
      if checkCurrent then
        begin
          if GetRegKeyValue(root, keys.ValueFromIndex[i], subKey) <> value then
            CreateRegValue(root, keys.ValueFromIndex[i], subKey, value);
        end
      else
        CreateRegValue(root, keys.ValueFromIndex[i], subKey, value);
    end;
end;

end.

