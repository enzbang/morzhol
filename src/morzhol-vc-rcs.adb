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

   use Morzhol.Strings;

   Diff_Rev_Opt      : constant String := "-r";
   Ci_Author_Opt     : constant String := "-w";

   Ci_Command        : aliased constant String := "ci";
   Ci_Opt            : aliased String := "-u";

   Co_Command        : aliased constant String := "co";
   Co_Opt            : aliased String := "-l";

   Log_Command       : aliased constant String := "rlog";
   Log_Total_Rev_Opt : aliased String := "-h";

   Diff_Command      : aliased constant String := "rcsdiff";

   function Commit
     (Engine           : in RCS;
      Filename         : in String;
      Message          : in String;
      Author           : in String;
      Initial_Revision : in Boolean) return Boolean;
   --  Commit or Add if Initial_Revision is True

   -----------
   --  Add  --
   -----------

   function Add
     (Engine   : in RCS;
      Filename : in String;
      Author   : in String := "") return Boolean
   is
      Local_RCS_Dir : constant String :=
                        Directories.Containing_Directory (Filename)
                        & OS.Directory_Separator & "RCS";
   begin
      if not Directories.Exists (Local_RCS_Dir) then
         Directories.Create_Directory (Local_RCS_Dir);
      end if;

      return Commit
        (Engine           => Engine,
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
      Author   : in String := "") return Boolean is
   begin
      return Commit
        (Engine           => Engine,
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
      Initial_Revision : in Boolean) return Boolean
   is
      pragma Unreferenced (Engine);

      use type Expect.Expect_Match;

      Pd          : Expect.Process_Descriptor;
      Result      : Expect.Expect_Match;

      RCS_File    : aliased String := Filename;
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

      Expect.Non_Blocking_Spawn
        (Descriptor => Pd,
         Command    => Ci_Command,
         Args       =>
           OS_Lib.Argument_List'
           (1 => Ci_Opt'Access,
            2 => RCS_Author,
            3 => RCS_Message,
            4 => RCS_File'Unchecked_Access),
         Err_To_Out => True);

      OS_Lib.Free (RCS_Author);
      OS_Lib.Free (RCS_Message);

      Expect.Expect (Pd, Result, "done");
      Expect.Close (Pd);

      return Result = 1;

   exception
      when Expect.Invalid_Process | Expect.Process_Died =>
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

      RCS_File : aliased String := Filename;
      Rev1     : aliased String := Diff_Rev_Opt & From_Version;
      Rev2     : aliased String := Diff_Rev_Opt & To_Version;

      Status : aliased Integer;
      Result : Unbounded_String;
   begin
      Result := +Expect.Get_Command_Output
        (Command    => Diff_Command,
         Arguments  =>
           OS_Lib.Argument_List'(1 => Rev1'Unchecked_Access,
                                 2 => Rev2'Unchecked_Access,
                                 3 => RCS_File'Unchecked_Access),
         Input      => "",
         Status     => Status'Access,
         Err_To_Out => True);

      return -Result;
   exception
      when Expect.Invalid_Process | Expect.Process_Died =>
         return -Result;
   end Diff;

   ---------------
   --  Get_Log  --
   ---------------

   function Get_Log
     (Engine   : in RCS;
      Filename : in String;
      Limit    : in Natural := 0) return Log
   is
      pragma Unreferenced (Engine, Limit);
      use type Expect.Expect_Match;

      function Get_Revision_Number return Natural;
      --  Return the number of revision for that file
      --  or 0 if an error has occured

      RCS_File : aliased String := Filename;

      -------------------------
      -- Get_Revision_Number --
      -------------------------

      function Get_Revision_Number return Natural is

         Pd      : Expect.Process_Descriptor;
         Matched : Regpat.Match_Array (Regpat.Match_Count range 0 .. 1);
         Result  : Expect.Expect_Match;
      begin
         Expect.Non_Blocking_Spawn
           (Descriptor => Pd,
            Command    => Log_Command,
            Args       => OS_Lib.Argument_List'
              (1 => Log_Total_Rev_Opt'Access,
               2 => RCS_File'Unchecked_Access),
            Err_To_Out => True);

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

      Revision_Number : constant Natural := Get_Revision_Number;

      Pd       : Expect.Process_Descriptor;
      File_Log : Log (1 .. Revision_Number);
      Current  : Positive := 1;

   begin
      if Revision_Number = 0 then
         return File_Log;
      end if;

      Expect.Non_Blocking_Spawn
        (Descriptor => Pd,
         Command    => Log_Command,
         Args       => OS_Lib.Argument_List'(1 => RCS_File'Unchecked_Access),
         Err_To_Out => True);

      loop
         Read_Out : declare
            Matched : Regpat.Match_Array (Regpat.Match_Count range 0 .. 4);
            Result  : Expect.Expect_Match;
            CL      : Commit_Log;
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

   exception
      when Expect.Invalid_Process | Expect.Process_Died =>
         return File_Log;
   end Get_Log;

   ------------
   --  Lock  --
   ------------

   function Lock
     (Engine : in RCS; Filename : in String) return Boolean
   is
      pragma Unreferenced (Engine);
      use type Expect.Expect_Match;

      Local_RCS_Dir : constant String :=
                        Directories.Containing_Directory (Filename)
                        & OS.Directory_Separator & "RCS";

      RCS_File  : aliased String := Filename;
      Pd        : Expect.Process_Descriptor;
      Result    : Expect.Expect_Match;
      Matched   : Regpat.Match_Array (Regpat.Match_Count range 0 .. 1);

   begin
      if not Directories.Exists (Local_RCS_Dir) then
         Directories.Create_Directory (Local_RCS_Dir);
      end if;

      Expect.Non_Blocking_Spawn
        (Descriptor => Pd,
         Command    => Co_Command,
         Args       => OS_Lib.Argument_List'(1 => Co_Opt'Access,
                                             2 => RCS_File'Unchecked_Access),
         Err_To_Out => True);

      Expect.Expect (Pd, Result, "locked.*\n.*done.*", Matched);

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

   function Remove (Engine : in RCS; Filename : in String) return Boolean is
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
