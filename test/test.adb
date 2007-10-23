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

with Morzhol.Strings;
with Morzhol.VC.RCS;
with Ada.Text_IO;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Command_Line;
with Morzhol.VC;

procedure Test is

   use Ada;
   use Ada.Text_IO;
   use Ada.Command_Line;

   use Morzhol.Strings;
   use Morzhol.VC.RCS;

   VCS_Engine : RCS;
begin

   Set_Exit_Status (Failure);

   if HTML_To_Text
     ("<a href='http://morzhol.google.code.com'>Morzhol</a>") /= "Morzhol"
     or else HTML_To_Text ("A <em>small</em> test") /= "A small test"
     or else HTML_To_Text
       ("for <br /><hr /><br /> html_to_text") /= "for  html_to_text"
   then
      Put_Line ("HTML_To_Text Error");
      return;
   end if;

   Create_Test_File : declare
      File_To_Create : File_Type;
   begin

      if Directories.Exists ("test/RCS") then
         Directories.Delete_Tree ("test/RCS");
      end if;

      if Directories.Exists ("test/RCS_Test") then
         Directories.Delete_File ("test/RCS_Test");
      end if;

      Create (File => File_To_Create,
              Mode => Out_File,
              Name => "test/RCS_Test");

      Put (File_To_Create, "test");
      Close (File_To_Create);
   end Create_Test_File;

   if not Lock (VCS_Engine, "test/RCS_Test") then
      --  Try to add

      if not Add (VCS_Engine, "test/RCS_Test", "initial_author")
        or else not Lock (VCS_Engine, "test/RCS_Test")
      then
         Put_Line ("Can not Lock RCS Test !");
         return;
      end if;
   end if;

   Change_Test_File : declare
     File_To_Change : File_Type;
   begin
      Open (File => File_To_Change,
            Mode => Out_File,
            Name => "test/RCS_Test");

      Put (File_To_Change, "modification");
      Close (File_To_Change);

   end Change_Test_File;

   if not Commit (Engine   => VCS_Engine,
                  Filename => "test/RCS_Test",
                  Message  => "first commit by test.adb",
                  Author   => "author") then
      Put_Line ("Commit failure");
      return;
   end if;

   declare
      File_Log : constant Morzhol.VC.Log :=
                   Get_Log (VCS_Engine, "test/RCS_Test");
   begin

      if Diff (VCS_Engine, "test/RCS_Test",
         -File_Log (2).Revision, -File_Log (1).Revision) /=
       "==================================================================="
        & ASCII.LF & "RCS file: test/RCS/RCS_Test,v"
        & ASCII.LF & "retrieving revision 1.1"
        & ASCII.LF & "retrieving revision 1.2"
        & ASCII.LF & "diff -r1.1 -r1.2"
        & ASCII.LF & "1c1"
        & ASCII.LF & "< test"
        & ASCII.LF & "---"
        & ASCII.LF & "> modification"
        or else -File_Log (1).Author /= "author"
        or else -File_Log (2).Author /= "initial_author"
      then
         Put_Line ("Diff error !");
         return;
      end if;
   end;

   Put_Line ("OK. All tests passed !");
   Set_Exit_Status (Success);
exception
   when E : others =>
      Put_Line (Exceptions.Exception_Information (E));
end Test;
