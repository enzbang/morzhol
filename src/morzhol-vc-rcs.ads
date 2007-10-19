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

package Morzhol.VC.RCS is

   type RCS is new VCS with null record;

   function Lock
     (Engine : in RCS; Filename : in String) return Boolean;
   --  Lock the files you check out, using the -l option

   function Commit
     (Engine   : in RCS;
      Filename : in String;
      Message  : in String;
      Author   : in String := "")
     return Boolean;
   --  Checking in the changes

   function Add
     (Engine   : in RCS;
      Filename : in String;
      Author   : in String := "")
      return Boolean;
   --  Do the initial check-in

   function Remove (Engine : in RCS; Filename : in String) return Boolean;
   --  Remove a file

   function Get_Log
     (Engine   : in RCS;
      Filename : in String;
      Limit    : in Natural := 0)
     return Log;

   function Diff
     (Engine       : in RCS;
      Filename     : in String;
      From_Version : in String;
      To_Version   : in String)
     return String;
   --  Returns the diff

end Morzhol.VC.RCS;
