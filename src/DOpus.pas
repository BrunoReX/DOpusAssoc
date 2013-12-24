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

unit DOpus;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Registry, FGL,
  RegFunc;

type
  TBooleanList = specialize TFPGList<Boolean>;
  TDOpus = class
  protected
    function GetDOpusPath(): String;
    function GetOpenCommand(): String;
    function GetIcon(): String;
  public
    const assocKey: String = 'DOpusAssoc';
    const bakSubKey: String = 'DOpusAssoc_bak';
    function CreateAssocKey(): Boolean;
    function GetExtensions(): TStringList;
    function CheckAssocValue(value: String): Boolean;
    procedure SaveAssociations(exts: TBooleanList);
  end;

implementation

function TDOpus.GetExtensions(): TStringList;
var
  Extensions: TStringList;
begin
  Extensions := TStringList.Create;
  Extensions.Add('.000');
  Extensions.Add('.001');
  Extensions.Add('.7z');
  Extensions.Add('.arj');
  Extensions.Add('.bz2');
  Extensions.Add('.bzip2');
  Extensions.Add('.cab');
  Extensions.Add('.cb7');
  Extensions.Add('.cbr');
  Extensions.Add('.cbz');
  Extensions.Add('.cpio');
  Extensions.Add('.deb');
  Extensions.Add('.dmg');
  Extensions.Add('.gz');
  Extensions.Add('.gzip');
  Extensions.Add('.hfs');
  Extensions.Add('.iso');
  Extensions.Add('.lha');
  Extensions.Add('.lhz');
  Extensions.Add('.lzma');
  Extensions.Add('.lzma86');
  Extensions.Add('.r00');
  Extensions.Add('.r01');
  Extensions.Add('.rar');
  Extensions.Add('.rpm');
  Extensions.Add('.swm');
  Extensions.Add('.tar');
  Extensions.Add('.tbz');
  Extensions.Add('.tbz2');
  Extensions.Add('.tgz');
  Extensions.Add('.tlz');
  Extensions.Add('.tlzma');
  Extensions.Add('.tpz');
  Extensions.Add('.wim');
  Extensions.Add('.xar');
  Extensions.Add('.z');
  Extensions.Add('.zipx');

  Result := Extensions;
end;

function TDOpus.GetDOpusPath(): String;
var
  regKeys: TStringList;
  regSubKeys: TStringList;
  regRoots: array of HKEY;
  MAX_KEYS: Integer;
  rVal: String = '';
  const EXE_NAME: String = 'dopus.exe';
  const GUID: String = '{5D4F167D-CCC8-413E-A6EE-F2FABBBBF50D}';
begin
  regKeys := TStringList.Create;
  regSubKeys := TStringList.Create;

  regKeys.Add('SOFTWARE\Microsoft\Windows\CurrentVersion\App Paths\' + EXE_NAME);
  regKeys.Add('SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\' + GUID);

  regSubKeys.Add('');
  regSubKeys.Add('InstallLocation');

  MAX_KEYS := regKeys.Count;
  SetLength(regRoots, MAX_KEYS);

  regRoots[0] := HKEY_LOCAL_MACHINE;
  regRoots[1] := HKEY_LOCAL_MACHINE;

  if RegKeyExists(regRoots[0], regKeys.ValueFromIndex[0]) then
    // HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion\App Paths\dopus.exe
    rVal := GetRegKeyValue(regRoots[0], regKeys.ValueFromIndex[0], regSubKeys.ValueFromIndex[0])
  else if RegKeyExists(regRoots[1], regKeys.ValueFromIndex[1]) then
    begin
      //HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\{5D4F167D-CCC8-413E-A6EE-F2FABBBBF50D}
      rVal := GetRegKeyValue(regRoots[1], regKeys.ValueFromIndex[1], regSubKeys.ValueFromIndex[1]);

      if (rVal <> '') then
        rVal := rVal + '\' + EXE_NAME;
    end;

  Result := rVal;
end;

function TDOpus.GetOpenCommand(): String;
var
  regRoot: HKEY;
  regKey: String;
  rVal: String = '';
begin
  regRoot := HKEY_CLASSES_ROOT;
  regKey := 'OpusZip\shell\open\command';

  if RegKeyExists(regRoot, regKey) then
    // HKEY_CLASSES_ROOT\OpusZip\shell\open\command
    rVal := GetRegKeyValue(regRoot, regKey)
  else
    begin
      rVal := GetDOpusPath();

      if (rVal <> '') then
        rVal := '"' + GetDOpusPath() + '" /cmd go "%1" existinglister newtab=tofront';
    end;

  Result := rVal;
end;

function TDOpus.GetIcon(): String;
var
  regRoot: HKEY;
  regKey: String;
  rVal: String = '';
begin
  regRoot := HKEY_CLASSES_ROOT;
  regKey := 'OpusZip\DefaultIcon';

  if RegKeyExists(regRoot, regKey) then
    // HKEY_CLASSES_ROOT\OpusZip\DefaultIcon
    rVal := GetRegKeyValue(regRoot, regKey)
  else
    begin
      rVal := GetDOpusPath();

      if (rVal <> '') then
        rVal := GetDOpusPath() + ',-112';
    end;

  Result := rVal;
end;

function TDOpus.CreateAssocKey(): Boolean;
var
  regRoot: HKEY;
  regKey: String;
  regSubKey: String;
  regSubKey2: String;
  keyDesc: String = 'Compressed File';
begin
  Result := False;

  regRoot := HKEY_CLASSES_ROOT;
  regKey := assocKey;
  regSubKey := regKey + '\shell\open\command';
  regSubKey2 := regKey + '\DefaultIcon';

  if not RegKeyExists(regRoot, regKey) then
    CreateRegKey(regRoot, regKey);

  if not RegKeyExists(regRoot, regSubKey) then
    CreateRegKey(regRoot, regSubKey);

  if not RegKeyExists(regRoot, regSubKey2) then
    CreateRegKey(regRoot, regSubKey2);

  if GetRegKeyValue(regRoot, regKey) <> keyDesc then
    CreateRegValue(regRoot, regKey, '', keyDesc);

  if GetRegKeyValue(regRoot, regSubKey) <> GetOpenCommand() then
    CreateRegValue(regRoot, regSubKey, '', GetOpenCommand());

  if GetRegKeyValue(regRoot, regSubKey2) <> GetIcon() then
    CreateRegValue(regRoot, regSubKey2, '', GetIcon());

  Result := True;
end;

function TDOpus.CheckAssocValue(value: String): Boolean;
  begin
    if value <> assocKey then
      Result := False
    else
      Result := True;
  end;

procedure TDOpus.SaveAssociations(exts: TBooleanList);
var
  i: Integer;
  root: HKEY;
  curValue: String;
  bakValue: String;
  exts2: TStringList;
begin
  exts2 := GetExtensions();
  root := HKEY_CLASSES_ROOT;

  for i := 0 to exts.Count-1 do
    begin
      curValue := '';
      bakValue := '';

      if exts[i] then
        begin
          curValue := GetRegKeyValue(root, exts2[i]);

          if curValue <> assocKey then // Check if the current value is not already the right one
            begin
              CreateRegValue(root, exts2[i], '', assocKey);

              if curValue <> '' then
                CreateRegValue(root, exts2[i], bakSubKey, curValue); // Backup current value
            end;
        end
      else
        begin
          curValue := GetRegKeyValue(root, exts2[i]);
          bakValue := GetRegKeyValue(root, exts2[i], bakSubKey);

          if curValue = assocKey then
            if (bakValue = '') then
              DeleteRegValue(root, exts2[i], '')
            else
              begin
                CreateRegValue(root, exts2[i], '', bakValue); // Restore backed up value
                DeleteRegValue(root, exts2[i], bakSubKey); // Delete backup
              end;
        end;
    end;
end;

end.

