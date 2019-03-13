--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded;

package Files is

   use Ada.Strings.Unbounded;

   package Collection_Lists is
      new Ada.Containers.Doubly_Linked_Lists
     (Ada.Strings.Unbounded.Unbounded_String);

   subtype Collection_List is Collection_Lists.List;

   procedure Append_Collections (Collection : in out Collection_List;
                                 Directory  : in     String);
   --  Scan Directory adding valid databases to Collection.

end Files;

