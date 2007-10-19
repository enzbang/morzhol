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

with Ada.Text_IO;
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

   Cmd : constant String := "cmd.exe";

   Cmd_Option : aliased String := "/c";
   Sh_Option  : aliased String := "sh";

   Ci_Command : aliased String := "ci";
   Ci_Opt     : aliased String := "-u";

   Co_Command : aliased String := "co";
   Co_Opt     : aliased String := "-l";

   Log_Command       : aliased String := "rlog";
   Log_Total_Rev_Opt : aliased String := "-h";

   -----------
   --  Add  --
   -----------

   function Add (Engine : in RCS; Filename : in String) return Boolean
   is
   begin
      return Commit (Engine, Filename, "File : " & Filename);
   end Add;

   --------------
   --  Commit  --
   --------------

   function Commit
     (Engine   : in RCS;
      Filename : in String;
      Message  : in String)
     return Boolean
   is
      pragma Unreferenced (Engine);

      Pd      : Expect.Process_Descriptor;
      Matched : Regpat.Match_Array (Regpat.Match_Count range 0 .. 1);
      Result  : Expect.Expect_Match;

      File_To_Commit : constant OS_Lib.String_Access := new String'(Filename);

   begin
      Launch_External : begin
         if Is_Windows then
            Expect.Non_Blocking_Spawn
              (Pd, Cmd,
               OS_Lib.Argument_List'(1 => Cmd_Option'Access,
                                     2 => Sh_Option'Access,
                                     3 => Ci_Command'Access,
                                     4 => File_To_Commit));
         else
            Expect.Non_Blocking_Spawn
              (Pd, Ci_Command,
               OS_Lib.Argument_List'(1 => Ci_Opt'Access,
                                     2 => File_To_Commit),
              Err_To_Out => True);

         end if;

         Expect.Send (Pd, Message);
         Expect.Send (Pd, ".");
      end Launch_External;

      Read_Out : begin
         Expect.Expect
           (Pd, Result, ".*\n.*\n.*", Matched);
         Text_IO.Put_Line (Expect.Expect_Out (Pd));
      exception
         when Expect.Process_Died =>
            return False;
      end Read_Out;

      case Result is
         when 1 => Text_IO.Put_Line (Expect.Expect_Out (Pd));
         when Expect.Expect_Timeout =>
            Text_IO.Put_Line (Expect.Expect_Out (Pd));
            return False;
         when others =>
            Text_IO.Put_Line (Expect.Expect_Out (Pd));
            null;
      end case;
      return True;
   end Commit;

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
      Targetted_File : constant OS_Lib.String_Access := new String'(Filename);

      function Get_Revision_Number return Natural;
      --  Return the number of revision for that file

      function Get_Revision_Number return Natural is
         Pd      : Expect.Process_Descriptor;
         Matched : Regpat.Match_Array (Regpat.Match_Count range 0 .. 1);
         Result  : Expect.Expect_Match;
      begin
         if Is_Windows then
            Expect.Non_Blocking_Spawn
              (Pd, Cmd,
               OS_Lib.Argument_List'(1 => Cmd_Option'Access,
                                     2 => Sh_Option'Access,
                                     3 => Log_Command'Access,
                                     4 => Log_Total_Rev_Opt'Access,
                                     5 => Targetted_File));
         else
            Expect.Non_Blocking_Spawn
              (Pd, Log_Command,
               OS_Lib.Argument_List'(1 => Log_Total_Rev_Opt'Access,
                                     2 => Targetted_File),
               Err_To_Out => True);
         end if;

         Read_Out : begin
            Expect.Expect
              (Pd, Result, "total revisions: (.*)", Matched);
         exception
            when Expect.Process_Died =>
               return 0;
         end Read_Out;
         case Result is
            when 1 => return Natural'Value
               (Expect.Expect_Out (Pd)
                  (Matched (1).First .. Matched (1).Last));
            when Expect.Expect_Timeout =>
               Text_IO.Put_Line (Expect.Expect_Out (Pd));
               return 0;
            when others =>
               Text_IO.Put_Line (Expect.Expect_Out (Pd));
               null;
         end case;

         return 1;
      end Get_Revision_Number;

      Pd      : Expect.Process_Descriptor;
      Matched : Regpat.Match_Array (Regpat.Match_Count range 0 .. 4);
      Result  : Expect.Expect_Match;

      Revision_Number : constant Natural := Get_Revision_Number;
      File_Log : Log (1 .. Revision_Number);
      Current  : Positive := 1;

   begin
      if Is_Windows then
         Expect.Non_Blocking_Spawn
           (Pd, Cmd,
            OS_Lib.Argument_List'(1 => Cmd_Option'Access,
                                  2 => Sh_Option'Access,
                                  3 => Log_Command'Access,
                                  4 => Targetted_File));
      else
         Expect.Non_Blocking_Spawn
           (Pd, Log_Command,
            OS_Lib.Argument_List'(1 => Targetted_File),
            Err_To_Out => True);
      end if;

      loop
         Read_Out : declare
            CL : Commit_Log;
         begin
            Expect.Expect
              (Pd, Result,
               "\nrevision ([1-9\.]+)\ndate: (.*?);"
                 & ".*author: ([a-zA-Z]+);.*\n(.*)",
               Matched);
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
         exception
            when Expect.Process_Died =>
               exit;
         end Read_Out;
      end loop;
      return File_Log;
   end Get_Log;

   ------------
   --  Lock  --
   ------------

   function Lock
     (Engine : in RCS; Filename : in String) return Boolean is
      pragma Unreferenced (Engine);

      Local_RCS_Dir : constant String
        := Directories.Containing_Directory (Filename)
        & Directory_Separator & "RCS";

      Pd        : Expect.Process_Descriptor;
      Result    : Expect.Expect_Match;
      Matched   : Regpat.Match_Array (Regpat.Match_Count range 0 .. 1);
      File_To_Lock : constant OS_Lib.String_Access := new String'(Filename);

   begin
      if not Directories.Exists (Local_RCS_Dir) then
         Directories.Create_Directory (Local_RCS_Dir);
      end if;

      Launch_External : begin
         if Is_Windows then
            Expect.Non_Blocking_Spawn
              (Pd, Cmd,
               OS_Lib.Argument_List'(1 => Cmd_Option'Access,
                                     2 => Sh_Option'Access,
                                     3 => Co_Command'Access,
                                     4 => Co_Opt'Access,
                                     5 => File_To_Lock),
              Err_To_Out => True);
         else
            Expect.Non_Blocking_Spawn
              (Pd, "co",
               OS_Lib.Argument_List'(1 => Co_Opt'Access,
                                     2 => File_To_Lock),
               Err_To_Out => True);
         end if;
      end Launch_External;

      Read_Out : begin
         Expect.Expect
           (Pd, Result, "locked", Matched);
      exception
         when Expect.Process_Died =>
            return False;
      end Read_Out;

      case Result is
         when 1 => Text_IO.Put_Line (Expect.Expect_Out (Pd));
            return True;
         when Expect.Expect_Timeout =>
            return False;
         when others =>
            null;
      end case;
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
