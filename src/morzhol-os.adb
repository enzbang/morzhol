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

with Ada.Characters.Handling;
with Ada.Directories;

package body Morzhol.OS is

   -------------
   -- Compose --
   -------------

   function Compose (Containing_Directory, Path : in String) return String is
   begin
      if (Path'Length > 1
          and then (Path (Path'First) = '/'
                    or else Path (Path'First) = Directory_Separator))
        or else (Path'Length > 3
                 and then Characters.Handling.Is_Letter (Path (Path'First))
                 and then Path (Path'First + 1 .. Path'First + 2) = ":\")
      then
         --  Absolute Path
         return Path;

      elsif Path'Length > 2
            and then
              (Path (Path'First .. Path'First + 1) = "./"
               or else Path (Path'First .. Path'First + 1) = ".\")
      then
         return Directories.Compose
           (Containing_Directory, Path (Path'First + 2 .. Path'Last));

      else
         return Directories.Compose (Containing_Directory, Path);
      end if;
   end Compose;

end Morzhol.OS;
