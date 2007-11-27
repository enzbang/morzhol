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

with Ada.Calendar.Formatting;
with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Text_IO;

package body Morzhol.Logs is

   use Ada;

   Is_Activated : array (Log_Level) of Boolean := (others => True);

   Log          : Text_IO.File_Type;
   Log_Use_File : Boolean := False;  --  Default to stdout

   protected Semaphore is
      entry Get;
      procedure Release;
   private
      Free : Boolean := True;
   end Semaphore;

   --------
   -- NV --
   --------

   function NV (Name, Value : in String) return String is
   begin
      return Name & "=""" & Value & '"';
   end NV;

   function NV (Name : in String; Value : in Integer) return String is
   begin
      return NV
        (Name,
         Strings.Fixed.Trim (Integer'Image (Value), Strings.Left));
   end NV;

   ---------------
   -- Semaphore --
   ---------------

   protected body Semaphore is

      ---------
      -- Get --
      ---------

      entry Get when Free is
      begin
         Free := False;
      end Get;

      -------------
      -- Release --
      -------------

      procedure Release is
      begin
         Free := True;
      end Release;

   end Semaphore;

   ---------
   -- Set --
   ---------

   procedure Set (Kind : in Log_Level; Activated : in Boolean) is
   begin
      Is_Activated (Kind) := Activated;
   end Set;

   --------------
   -- Set_File --
   --------------

   procedure Set_File (Filename : in String) is
   begin
      Text_IO.Create (File => Log,
                      Mode => Text_IO.Append_File,
                      Name => Filename);
      Log_Use_File := True;
   end Set_File;

   -----------
   -- Write --
   -----------

   procedure Write
     (Name    : in Module_Name;
      Content : in String;
      Kind    : in Log_Level := Information)
   is
   begin
      if Is_Activated (Kind) then
         Semaphore.Get;

         if Log_Use_File then

            Text_IO.Put_Line
              (Log,
               "[" & Calendar.Formatting.Image (Calendar.Clock) & "] ["
               & Characters.Handling.To_Upper (String (Name)) & "] ["
               & Log_Level'Image (Kind) & "] - " & Content);
            Text_IO.Flush (Log);

         else
            if Kind = Warnings or else Kind = Error then
               Text_IO.Put (Log_Level'Image (Kind) & " ");
            end if;
            Text_IO.Put_Line (Content);
            Text_IO.Flush;
         end if;
         Semaphore.Release;
      end if;
   exception
      when others =>
         --  ??? what else to do ?
         Semaphore.Release;
   end Write;

begin -- V2P.Logs
   Text_IO.Create (Log, Mode => Text_IO.Append_File, Name => "v2p.log");
end Morzhol.Logs;
