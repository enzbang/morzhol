------------------------------------------------------------------------------
--                                Morzhol                                   --
--                                                                          --
--                           Copyright (C) 2008                             --
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

with Ada.Environment_Variables;

package Morzhol.OS is

   use Ada;

   Is_Windows          : constant Boolean :=
                           Environment_Variables.Exists ("OS") and then
                           Environment_Variables.Value ("OS") = "Windows_NT";

   Directory_Separator : constant Character;

   function Compose (Containing_Directory, Path : in String) return String;
   --  Returns Containing_Directory & Directory_Separator & Path if PATH is
   --  relative, otherwise it returns Path. Removes any trailing directory
   --  separator.

   function Is_Directory_Separator (C : in Character) return Boolean;
   pragma Inline (Is_Directory_Separator);
   --  Returns true is C is a directory separator. Note that on Windows
   --  both / and \ are supported.

private

   subtype Windows_Host is Boolean;

   type DS_Array is array (Windows_Host) of Character;

   DS : DS_Array := DS_Array'(True => '\', False => '/');

   Directory_Separator : constant Character := DS (Is_Windows);

end Morzhol.OS;
