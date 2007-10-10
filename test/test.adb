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
with Ada.Command_Line;

procedure Test is

   use Ada;
   use Ada.Text_IO;

   use Morzhol.Strings;
   use Ada.Command_Line;
   use Morzhol.VC.RCS;

   VCS_Engine : RCS;
begin

   if HTML_To_Text ("<a href='http://morzhol.google.code.com'>Morzhol</a>")
     = "Morzhol"
     and then HTML_To_Text ("A <em>small</em> test") = "A small test"
     and then HTML_To_Text ("for <br /><hr /><br /> html_to_text")
     = "for  html_to_text"
   then
      Ada.Text_IO.Put_Line ("Ok - 1");
      Set_Exit_Status (Success);
   else
      Ada.Text_IO.Put_Line ("Error");
      Set_Exit_Status (Failure);
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

   if Lock (VCS_Engine, "test/RCS_Test") then
      Ada.Text_IO.Put_Line ("Ok - 2");
      Set_Exit_Status (Success);
   else
      --  Try to add

      if Add (VCS_Engine, "test/RCS_Test") and
        then Lock (VCS_Engine, "test/RCS_Test")
      then
           Ada.Text_IO.Put_Line ("Ok - 3");
           Set_Exit_Status (Success);
        else
           Ada.Text_IO.Put_Line ("Error - :(");
           Set_Exit_Status (Failure);
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


   if Commit (VCS_Engine, "test/RCS_Test", "first commit by test.adb") then
      Ada.Text_IO.Put_Line ("Ok - 4");
      Set_Exit_Status (Success);
   else
      Ada.Text_IO.Put_Line ("Error - :(");
      Set_Exit_Status (Failure);
      return;
   end if;
end Test;
