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

with Ada.Characters.Handling;

package body Morzhol.OS is

   -------------
   -- Compose --
   -------------

   function Compose (Containing_Directory, Path : in String) return String is

      function CD_Sep return String;
      --  Returns the Containing_Directory with an ending directory separator

      ------------
      -- CD_Sep --
      ------------

      function CD_Sep return String is
         CD : String renames Containing_Directory;
      begin
         if CD'Length > 1
           and then
           (CD (CD'Last) = '/' or else CD (CD'Last) = Directory_Separator)
         then
            return CD;
         else
            return CD & Directory_Separator;
         end if;
      end CD_Sep;

      Last : Natural := Path'Last;

   begin
      --  Removes any trailing directory separator

      while Last /= 0 and then Is_Directory_Separator (Path (Last)) loop
         Last := Last - 1;
      end loop;

      if (Path'Length > 1
          and then Is_Directory_Separator (Path (Path'First)))
        or else (Path'Length > 3
                 and then Characters.Handling.Is_Letter (Path (Path'First))
                 and then Path (Path'First + 1 .. Path'First + 2) = ":\")
      then
         --  Absolute Path
         return Path (Path'First .. Last);

      elsif Path'Length > 2
            and then
              (Path (Path'First .. Path'First + 1) = "./"
               or else Path (Path'First .. Path'First + 1) = ".\")
      then
         return CD_Sep & Path (Path'First + 2 .. Last);

      else
         return CD_Sep & Path (Path'First .. Last);
      end if;
   end Compose;

   ----------------------------
   -- Is_Directory_Separator --
   ----------------------------

   function Is_Directory_Separator (C : in Character) return Boolean is
   begin
      return C = '/' or else C = Directory_Separator;
   end Is_Directory_Separator;

end Morzhol.OS;
