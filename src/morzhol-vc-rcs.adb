------------------------------------------------------------------------------
--                                Morzhol                                   --
--                                                                          --
--                           Copyright (C) 2007                             --
--                      Pascal Obry - Olivier Ramonat                       --
--                                                                          --
--  This library is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.       --
------------------------------------------------------------------------------

with Ada.Directories;

with GNAT.Expect;
with GNAT.Regpat;
with GNAT.OS_Lib;

with Morzhol.OS;
with Morzhol.Strings;

package body Morzhol.VC.RCS is

   use Ada;
   use GNAT;

   use Morzhol.OS;
   use Morzhol.Strings;

   Cmd           : constant String := "cmd.exe";
   Diff_Rev_Opt  : constant String := "-r";
   Ci_Author_Opt : constant String := "-w";

   Cmd_Option : aliased String := "/c";
   Sh_Option  : aliased String := "sh";

   Ci_Command : aliased String := "ci";
   Ci_Opt     : aliased String := "-u";

   Co_Command : aliased String := "co";
   Co_Opt     : aliased String := "-l";

   Log_Command       : aliased String := "rlog";
   Log_Total_Rev_Opt : aliased String := "-h";

   Diff_Command : aliased String := "rcsdiff";

   function Commit
     (Engine           : in RCS;
      Filename         : in String;
      Message          : in String;
      Author           : in String;
      Initial_Revision : in Boolean)
     return Boolean;
   --  Commit or Add if Initial_Revision is True

   -----------
   --  Add  --
   -----------

   function Add
     (Engine   : in RCS;
      Filename : in String;
      Author   : in String := "")
      return Boolean
   is
      Local_RCS_Dir : constant String
        := Directories.Containing_Directory (Filename)
        & Directory_Separator & "RCS";
   begin
      if not Directories.Exists (Local_RCS_Dir) then
         Directories.Create_Directory (Local_RCS_Dir);
      end if;

      return Commit (Engine           => Engine,
                     Filename         => Filename,
                     Message          => "File : " & Filename,
                     Author           => Author,
                     Initial_Revision => True);
   end Add;

   --------------
   --  Commit  --
   --------------

   function Commit
     (Engine   : in RCS;
      Filename : in String;
      Message  : in String;
      Author   : in String := "")
     return Boolean
   is
   begin
      return Commit (Engine           => Engine,
                     Filename         => Filename,
                     Message          => Message,
                     Author           => Author,
                     Initial_Revision => False);
   end Commit;

   --------------
   --  Commit  --
   --------------

   function Commit
     (Engine           : in RCS;
      Filename         : in String;
      Message          : in String;
      Author           : in String;
      Initial_Revision : in Boolean)
     return Boolean
   is

      pragma Unreferenced (Engine);

      use type Expect.Expect_Match;

      Pd      : Expect.Process_Descriptor;
      Result  : Expect.Expect_Match;

      RCS_File    : OS_Lib.String_Access := new String'(Filename);
      RCS_Message : OS_Lib.String_Access;
      RCS_Author  : OS_Lib.String_Access;

   begin
      if Initial_Revision then
         RCS_Message := new String'("-t-" & Message);
      else
         RCS_Message := new String'("-m" & Message);
      end if;

      if Author /= "" then
         RCS_Author := new String'(Ci_Author_Opt & Author);
      end if;

      if Is_Windows then
         Expect.Non_Blocking_Spawn
           (Descriptor => Pd,
            Command    => Cmd,
            Args       => OS_Lib.Argument_List'(1 => Cmd_Option'Access,
                                                2 => Sh_Option'Access,
                                                3 => Ci_Command'Access,
                                                4 => Ci_Opt'Access,
                                                5 => RCS_Author,
                                                6 => RCS_Message,
                                                7 => RCS_File),
            Err_To_Out => True);
      else
         Expect.Non_Blocking_Spawn
           (Descriptor => Pd,
            Command    => Ci_Command,
            Args       => OS_Lib.Argument_List'(1 => Ci_Opt'Access,
                                                2 => RCS_Author,
                                                3 => RCS_Message,
                                                4 => RCS_File),
            Err_To_Out => True);
      end if;

      OS_Lib.Free (RCS_File);
      OS_Lib.Free (RCS_Author);

      Expect.Expect (Pd, Result, "done");
      Expect.Close (Pd);

      return Result = 1;

   exception
      when Expect.Invalid_Process | Expect.Process_Died =>
         OS_Lib.Free (RCS_File);
         OS_Lib.Free (RCS_Author);
         OS_Lib.Free (RCS_Message);
         return False;
   end Commit;

   ------------
   --  Diff  --
   ------------

   function Diff
     (Engine       : in RCS;
      Filename     : in String;
      From_Version : in String;
      To_Version   : in String)
     return String
   is
      pragma Unreferenced (Engine);

      RCS_File : OS_Lib.String_Access := new String'(Filename);
      Rev1     : OS_Lib.String_Access :=
                   new String'(Diff_Rev_Opt & From_Version);
      Rev2     : OS_Lib.String_Access :=
                   new String'(Diff_Rev_Opt & To_Version);

      Status : aliased Integer;
      Result : Unbounded_String;
   begin
      if Is_Windows then
         Result := +Expect.Get_Command_Output
           (Command    => Cmd,
            Arguments  => OS_Lib.Argument_List'(1 => Cmd_Option'Access,
                                                2 => Sh_Option'Access,
                                                3 => Diff_Command'Access,
                                                4 => Rev1,
                                                5 => Rev2,
                                                6 => RCS_File),
            Input      => "",
            Status     => Status'Access,
            Err_To_Out => True);
      else
         Result := +Expect.Get_Command_Output
           (Command    => Diff_Command,
            Arguments  => OS_Lib.Argument_List'(1 => Rev1,
                                                2 => Rev2,
                                                3 => RCS_File),
            Input      => "",
            Status     => Status'Access,
            Err_To_Out => True);

      end if;

      OS_Lib.Free (RCS_File);
      OS_Lib.Free (Rev1);
      OS_Lib.Free (Rev2);

      return -Result;
   exception
      when Expect.Invalid_Process | Expect.Process_Died =>
         OS_Lib.Free (RCS_File);
         OS_Lib.Free (Rev1);
         OS_Lib.Free (Rev2);

         return -Result;
   end Diff;

   ---------------
   --  Get_Log  --
   ---------------

   function Get_Log
     (Engine   : in RCS;
      Filename : in String;
      Limit    : in Natural := 0)
     return Log
   is
      pragma Unreferenced (Engine, Limit);
      use type Expect.Expect_Match;

      RCS_File : OS_Lib.String_Access := new String'(Filename);

      function Get_Revision_Number return Natural;
      --  Return the number of revision for that file
      --  or 0 if an error has occured

      function Get_Revision_Number return Natural is

         Pd      : Expect.Process_Descriptor;
         Matched : Regpat.Match_Array (Regpat.Match_Count range 0 .. 1);
         Result  : Expect.Expect_Match;
      begin
         if Is_Windows then
            Expect.Non_Blocking_Spawn
              (Descriptor => Pd,
               Command    => Cmd,
               Args       => OS_Lib.Argument_List'
                 (1 => Cmd_Option'Access,
                  2 => Sh_Option'Access,
                  3 => Log_Command'Access,
                  4 => Log_Total_Rev_Opt'Access,
                  5 => RCS_File),
               Err_To_Out => True);
         else
            Expect.Non_Blocking_Spawn
              (Descriptor => Pd,
               Command    => Log_Command,
               Args       => OS_Lib.Argument_List'
                 (1 => Log_Total_Rev_Opt'Access,
                  2 => RCS_File),
               Err_To_Out => True);
         end if;

         Expect.Expect
           (Descriptor => Pd,
            Result     => Result,
            Regexp     => "total revisions: (.*)",
            Matched    => Matched);

         if Result = 1 then
            return Natural'Value
              (Expect.Expect_Out (Pd)
                 (Matched (1).First .. Matched (1).Last));
         else
            return 0;
         end if;
      end Get_Revision_Number;

      Pd      : Expect.Process_Descriptor;
      Matched : Regpat.Match_Array (Regpat.Match_Count range 0 .. 4);
      Result  : Expect.Expect_Match;

      Revision_Number : constant Natural := Get_Revision_Number;
      File_Log : Log (1 .. Revision_Number);
      Current  : Positive := 1;

   begin

      if Revision_Number = 0 then
         return File_Log;
      end if;

      if Is_Windows then
         Expect.Non_Blocking_Spawn
           (Descriptor => Pd,
            Command    => Cmd,
            Args       => OS_Lib.Argument_List'(1 => Cmd_Option'Access,
                                                2 => Sh_Option'Access,
                                                3 => Log_Command'Access,
                                                4 => RCS_File),
            Err_To_Out => True);
      else
         Expect.Non_Blocking_Spawn
           (Descriptor => Pd,
            Command    => Log_Command,
            Args       => OS_Lib.Argument_List'(1 => RCS_File),
            Err_To_Out => True);
      end if;

      loop
         Read_Out : declare
            CL : Commit_Log;
         begin
            Expect.Expect
              (Descriptor => Pd,
               Result     => Result,
               Regexp     => "\nrevision ([1-9\.]+)\ndate: (.*?);"
               & ".*author: (.*?);.*\n(.*)",
               Matched    => Matched);

            CL.Revision :=
              +Expect.Expect_Out (Pd)
              (Matched (1).First .. Matched (1).Last);
            CL.Date :=
              +Expect.Expect_Out (Pd)
              (Matched (2).First .. Matched (2).Last);
            CL.Author :=
              +Expect.Expect_Out (Pd)
              (Matched (3).First .. Matched (3).Last);
            CL.Message :=
              +Expect.Expect_Out (Pd)
              (Matched (4).First .. Matched (4).Last);

            File_Log (Current) := CL;
            Current := Current + 1;
         end Read_Out;
      end loop;

      OS_Lib.Free (RCS_File);

      return File_Log;

   exception
      when Expect.Invalid_Process | Expect.Process_Died =>
         OS_Lib.Free (RCS_File);
         return File_Log;
   end Get_Log;

   ------------
   --  Lock  --
   ------------

   function Lock
     (Engine : in RCS; Filename : in String) return Boolean is
      pragma Unreferenced (Engine);
      use type Expect.Expect_Match;

      Local_RCS_Dir : constant String
        := Directories.Containing_Directory (Filename)
        & Directory_Separator & "RCS";

      Pd        : Expect.Process_Descriptor;
      Result    : Expect.Expect_Match;
      Matched   : Regpat.Match_Array (Regpat.Match_Count range 0 .. 1);

      RCS_File : OS_Lib.String_Access := new String'(Filename);

   begin
      if not Directories.Exists (Local_RCS_Dir) then
         Directories.Create_Directory (Local_RCS_Dir);
      end if;

      if Is_Windows then
         Expect.Non_Blocking_Spawn
           (Descriptor => Pd,
            Command    => Cmd,
            Args       => OS_Lib.Argument_List'(1 => Cmd_Option'Access,
                                                2 => Sh_Option'Access,
                                                3 => Co_Command'Access,
                                                4 => Co_Opt'Access,
                                                5 => RCS_File),
            Err_To_Out => True);
      else
         Expect.Non_Blocking_Spawn
           (Descriptor => Pd,
            Command    => Co_Command,
            Args       => OS_Lib.Argument_List'(1 => Co_Opt'Access,
                                                2 => RCS_File),
            Err_To_Out => True);
      end if;

      OS_Lib.Free (RCS_File);

      Expect.Expect
        (Pd, Result, "locked.*\n.*done.*", Matched);

      if Result = 1 then
         Expect.Close (Pd);
         return True;
      end if;

      return False;
   exception
      when Expect.Invalid_Process | Expect.Process_Died =>
      return False;
   end Lock;

   --------------
   --  Remove  --
   --------------

   function Remove (Engine : in RCS; Filename : in String) return Boolean
   is
      pragma Unreferenced (Engine);
   begin

      --  Nothing special to do here as RCS support only files

      if Directories.Exists (Filename) then
         Directories.Delete_File (Filename);
         return True;
      end if;

      return False;
   end Remove;
end Morzhol.VC.RCS;
