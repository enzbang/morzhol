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

package body Morzhol.Strings is

   ------------------
   -- HTML_To_Text --
   ------------------

   function HTML_To_Text (HTML_Source : in String) return String is

      Result  : Unbounded_String;
      Last    : Integer := HTML_Source'First;
      To_Skip : Natural := 0;

   begin
      for K in HTML_Source'Range loop
         if To_Skip /= 0 then
            To_Skip := To_Skip - 1;
         else
            if HTML_Source (K) = '<' then
               Append (Result, HTML_Source (Last .. K - 1));
               Search_End_Tag :
               for L in K + 1 .. HTML_Source'Last loop
                  if HTML_Source (L) = '>' then
                     To_Skip := L - K;
                     Last    := L + 1;
                     exit Search_End_Tag;
                  end if;
               end loop Search_End_Tag;
            end if;
         end if;
      end loop;
      return -Result;
   end HTML_To_Text;

end Morzhol.Strings;
