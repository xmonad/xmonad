import Test.QuickCheck

-- Our QC instances and properties:
import Instances
import Properties.Delete
import Properties.Failure
import Properties.Floating
import Properties.Focus
import Properties.GreedyView
import Properties.Insert
import Properties.Screen
import Properties.Shift
import Properties.Stack
import Properties.StackSet
import Properties.Swap
import Properties.View
import Properties.Workspace
import Properties.Layout.Full
import Properties.Layout.Tall

import System.Environment
import Text.Printf

import Control.Monad
import Control.Applicative

main :: IO ()
main = do
  arg <- fmap (drop 1) getArgs
  let n = if null arg then 100 else read $ head arg
      args = stdArgs { maxSuccess = n, maxSize = 100 }
      qc t = do
          c <- quickCheckWithResult args t
          case c of
            Success {} -> return True
            _ -> return False
      perform (s, t) = printf "%-35s: " s >> qc t
  n <- length . filter not <$> mapM perform tests
  unless (n == 0) (error (show n ++ " test(s) failed"))



tests =
  [("StackSet invariants", property prop_invariant)
  ,("empty: invariant",    property prop_empty_I)
  ,("empty is empty",      property prop_empty)
  ,("empty / current",     property prop_empty_current)
  ,("empty / member",      property prop_member_empty)


  ,("view : invariant",  property prop_view_I)
  ,("view sets current", property prop_view_current)
  ,("view idempotent",   property prop_view_idem)
  ,("view reversible",   property prop_view_reversible)

  ,("view is local",  property prop_view_local)

  ,("greedyView : invariant",  property prop_greedyView_I)
  ,("greedyView sets current", property prop_greedyView_current)
  ,("greedyView is safe",      property prop_greedyView_current_id)
  ,("greedyView idempotent",   property prop_greedyView_idem)
  ,("greedyView reversible",   property prop_greedyView_reversible)
  ,("greedyView is local",     property prop_greedyView_local)

  ,("peek/member",  property prop_member_peek)

  ,("index/length", property prop_index_length)

  ,("focus left : invariant",    property prop_focusUp_I)
  ,("focus master : invariant",  property prop_focusMaster_I)
  ,("focus right: invariant",    property prop_focusDown_I)
  ,("focusWindow: invariant",    property prop_focus_I)
  ,("focus left/master",         property prop_focus_left_master)
  ,("focus right/master",        property prop_focus_right_master)
  ,("focus master/master",       property prop_focus_master_master)
  ,("focusWindow master",        property prop_focusWindow_master)
  ,("focus left/right",          property prop_focus_left)
  ,("focus right/left",          property prop_focus_right)
  ,("focus all left",            property prop_focus_all_l)
  ,("focus all right",           property prop_focus_all_r)
  ,("focus down is local",       property prop_focus_down_local)
  ,("focus up is local",         property prop_focus_up_local)
  ,("focus master is local",     property prop_focus_master_local)
  ,("focus master idemp",        property prop_focusMaster_idem)

  ,("focusWindow is local", property prop_focusWindow_local)
  ,("focusWindow works"   , property prop_focusWindow_works)
  ,("focusWindow identity", property prop_focusWindow_identity)

  ,("findTag",           property prop_findIndex)
  ,("allWindows/member", property prop_allWindowsMember)
  ,("currentTag",        property prop_currentTag)

  ,("insert: invariant",    property prop_insertUp_I)
  ,("insert/new",           property prop_insert_empty)
  ,("insert is idempotent", property prop_insert_idem)
  ,("insert is reversible", property prop_insert_delete)
  ,("insert is local",      property prop_insert_local)
  ,("insert duplicates",    property prop_insert_duplicate)
  ,("insert/peek",          property prop_insert_peek)
  ,("insert/size",          property prop_size_insert)

  ,("delete: invariant",       property prop_delete_I)
  ,("delete/empty",            property prop_empty)
  ,("delete/member",           property prop_delete)
  ,("delete is reversible",    property prop_delete_insert)
  ,("delete is local",         property prop_delete_local)
  ,("delete/focus",            property prop_delete_focus)
  ,("delete  last/focus up",   property prop_delete_focus_end)
  ,("delete ~last/focus down", property prop_delete_focus_not_end)

  ,("filter preserves order", property prop_filter_order)

  ,("swapLeft",  property prop_swap_left)
  ,("swapRight", property prop_swap_right)

  ,("swapMaster: invariant",    property prop_swap_master_I)
  ,("swapUp: invariant" ,       property prop_swap_left_I)
  ,("swapDown: invariant",      property prop_swap_right_I)
  ,("swapMaster id on focus",   property prop_swap_master_focus)
  ,("swapUp id on focus",       property prop_swap_left_focus)
  ,("swapDown id on focus",     property prop_swap_right_focus)
  ,("swapMaster is idempotent", property prop_swap_master_idempotent)
  ,("swap all left",            property prop_swap_all_l)
  ,("swap all right",           property prop_swap_all_r)
  ,("swapMaster is local",      property prop_swap_master_local)
  ,("swapUp is local",          property prop_swap_left_local)
  ,("swapDown is local",        property prop_swap_right_local)

  ,("shiftMaster id on focus",        property prop_shift_master_focus)
  ,("shiftMaster is local",           property prop_shift_master_local)
  ,("shiftMaster is idempotent",      property prop_shift_master_idempotent)
  ,("shiftMaster preserves ordering", property prop_shift_master_ordering)

  ,("shift: invariant"    ,       property prop_shift_I)
  ,("shift is reversible" ,       property prop_shift_reversible)
  ,("shiftWin: invariant" ,       property prop_shift_win_I)
  ,("shiftWin is shift on focus", property prop_shift_win_focus)
  ,("shiftWin fix current" ,      property prop_shift_win_fix_current)
  ,("shiftWin identity",          property prop_shift_win_indentity)

  ,("floating is reversible" ,  property prop_float_reversible)
  ,("floating sets geometry" ,  property prop_float_geometry)
  ,("floats can be deleted",    property prop_float_delete)
  ,("screens includes current", property prop_screens)

  ,("differentiate works",     property prop_differentiate)
  ,("lookupTagOnScreen",       property prop_lookup_current)
  ,("lookupTagOnVisbleScreen", property prop_lookup_visible)
  ,("screens works",           property prop_screens_works)
  ,("renaming works",          property prop_rename1)
  ,("ensure works",            property prop_ensure)
  ,("ensure hidden semantics", property prop_ensure_append)

  ,("mapWorkspace id",      property prop_mapWorkspaceId)
  ,("mapWorkspace inverse", property prop_mapWorkspaceInverse)

  ,("mapLayout id",      property prop_mapLayoutId)
  ,("mapLayout inverse", property prop_mapLayoutInverse)

  ,("abort fails",            property prop_abort)
  ,("new fails with abort",   property prop_new_abort)

  ,("point within",           property prop_point_within)

  -- tall layout

  ,("tile 1 window fullsize", property prop_tile_fullscreen)
  ,("tiles never overlap",    property prop_tile_non_overlap)
  ,("split horizontal",       property prop_split_horizontal)
  ,("split vertical",         property prop_split_vertical)

  ,("pure layout tall",       property prop_purelayout_tall)
  ,("send shrink    tall",    property prop_shrink_tall)
  ,("send expand    tall",    property prop_expand_tall)
  ,("send incmaster tall",    property prop_incmaster_tall)

  -- full layout

  ,("pure layout full",       property prop_purelayout_full)
  ,("send message full",      property prop_sendmsg_full)
  ,("describe full",          property prop_desc_full)

  ,("describe mirror",        property prop_desc_mirror)

  -- resize hints
  ,("window resize hints: inc",      property prop_resize_inc)
  ,("window resize hints: inc all",  property prop_resize_inc_extra)
  ,("window resize hints: max",      property prop_resize_max)
  ,("window resize hints: max all ", property prop_resize_max_extra)

  ,("window aspect hints: fits", property prop_aspect_fits)
  ,("window aspect hints: shrinks ", property prop_aspect_hint_shrink)


  ,("pointWithin",        property prop_point_within)
  ,("pointWithin mirror", property prop_point_within_mirror)

  ] <>
  prop_laws_Stack
