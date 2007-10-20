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

package body Morzhol.VC is

   use Morzhol.Strings;

   function Image (Commit : in Commit_Log) return String;

   -----------
   -- Image --
   -----------

   function Image (Commit : in Commit_Log) return String is
   begin
      return -Commit.Revision & ", " & (-Commit.Date) & ", " & (-Commit.Author)
        & ", " & (-Commit.Message);
   end Image;

   -----------
   -- Image --
   -----------

   function Image (VCS_Log : in Log) return String is
      Result : Unbounded_String;
   begin
      for K in VCS_Log'Range loop
         Append (Result, Image (VCS_Log (K)));
      end loop;

      return -Result;
   end Image;

end Morzhol.VC;
