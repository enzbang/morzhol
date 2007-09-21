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
with Ada.Text_IO;
with Ada.Command_Line;

procedure Test is

   use Morzhol.Strings;
   use Ada.Command_Line;
begin

   if HTML_To_Text ("<a href='http://morzhol.google.code.com'>Morzhol</a>")
     = "Morzhol"
     and then HTML_To_Text ("A <em>small</em> test") = "A small test"
     and then HTML_To_Text ("for <br /><hr /><br /> html_to_text")
     = "for  html_to_text"
   then
      Ada.Text_IO.Put_Line ("Ok");
      Set_Exit_Status (Success);
   else
      Ada.Text_IO.Put_Line ("Error");
      Set_Exit_Status (Failure);
   end if;
end Test;
