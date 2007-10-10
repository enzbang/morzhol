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

package Morzhol.VC is

   type VCS is interface;

   function Commit
     (Engine   : in VCS;
      Filename : in String;
      Message  : in String)
     return Boolean is abstract;
   --  Checking in the changes. Message is the commit log message

   function Lock
     (Engine : in VCS; Filename : in String) return Boolean is abstract;
   --  Lock file

   function Add
     (Engine : in VCS; Filename : in String) return Boolean is abstract;
   --  Add a new file or do the Initial check-in

   function Remove
     (Engine : in VCS; Filename : in String) return Boolean is abstract;
   --  Remove a file

end Morzhol.VC;
