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

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;

with Morzhol.OS;

procedure Test_Compose is

   use Ada;
   use Ada.Command_Line;
   use Ada.Text_IO;
   use Morzhol;

   Test_Error : exception;

   procedure Test (Dir, Path, Expected : in String);
   --  Check Compose (Dir, Path), output should be Expected

   ----------
   -- Test --
   ----------

   procedure Test (Dir, Path, Expected : in String) is
   begin
      if OS.Compose (Dir, Path) /= Expected then
         raise Test_Error with
           "Compose ERROR: " & OS.Compose (Dir, Path)
           & ", expected " & Expected;
      end if;
   end Test;

begin
   Test ("/dir/subdir", "azerty", Expected => "/dir/subdir/azerty");
   Test ("/dir/subdir", "/azerty", Expected => "/azerty");
   Test ("/dir/subdir", "./azerty", Expected => "/dir/subdir/azerty");
   Test ("/dir/subdir", "db/database", Expected => "/dir/subdir/db/database");

   if OS.Is_Windows then
      Test ("/dir/subdir", "\azerty", Expected => "\azerty");
   end if;

   Test ("/dir/subdir", "c:\dir\azerty", Expected => "c:\dir\azerty");

   Put_Line ("OK. All tests passed !");
   Set_Exit_Status (Success);

exception
   when E : others =>
      Put_Line (Exceptions.Exception_Information (E));
      Set_Exit_Status (Failure);
end Test_Compose;
