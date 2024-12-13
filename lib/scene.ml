open Bogue
open Lwt.Infix
module W = Widget
module L = Layout
open Deck

let () = Printexc.record_backtrace true

let card_border =
  Style.(mk_border ~radius:8 (mk_line ~width:20 ~color:(255, 255, 255, 255) ()))

let name_border =
  Style.(mk_border ~radius:8 (mk_line ~width:2 ~color:(0, 0, 0, 255) ()))

let property_border =
  Style.(mk_border ~radius:100 (mk_line ~width:2 ~color:(0, 0, 0, 255) ()))

let white_bg = Style.(color_bg (255, 255, 255, 255))

(* Create a promise that will be resolved when a card is clicked *)
let promise, resolver = Lwt.task ()
let card_promise = ref promise
let card_resolver = ref resolver

(* Add a ref to track popup state *)
let popup_active = ref false

(* Add this near the top with other global references *)
let next_card_id = ref 0
let card_info = Hashtbl.create 10

(* Near the top with other global values *)
let properties =
  [
    ("Mediterranean Avenue", (134, 96, 42, 255));
    (* Brown *)
    ("Oriental Avenue", (170, 224, 250, 255));
    (* Light Blue *)
    ("PoopWalk", (37, 57, 150, 255));
    (* Dark Blue *)
    ("Illinois Avenue", (255, 40, 40, 255));
    (* Red *)
    ("Boardwalk", (37, 57, 150, 255));
    (* Dark Blue *)
  ]

(* layout varies by number of players... we can hardcode this cause we have *)
let create_property_card ~name ~bg_color ~is_popup ~clickable =
  let blue_bg = Style.(color_bg bg_color) in

  (* Card background *)
  let card =
    W.box ~w:400 ~h:628
      ~style:Style.(create ~background:blue_bg ~border:card_border ())
      ()
  in

  (* Property name components *)
  let card_label = W.label ~size:32 name in
  let name_box =
    W.box ~w:320 ~h:100
      ~style:Style.(create ~background:white_bg ~border:name_border ())
      ()
  in

  (* Rent info components *)
  let rent_info_line1 = W.label ~size:24 ~align:Center "Rent Details by" in
  let rent_info_line2 = W.label ~size:24 ~align:Center "# Properties Owned" in
  let rent_info =
    L.tower ~align:Center
      [ L.resident rent_info_line1; L.resident rent_info_line2 ]
  in

  (* Rent amount components *)
  let rent_label = W.label ~size:24 ~align:Center "1 Prop - 1M" in
  let one_property =
    W.box ~w:312 ~h:60
      ~style:Style.(create ~background:white_bg ~border:property_border ())
      ()
  in

  (* Compose layouts *)
  let name_layout =
    L.superpose ~center:true [ L.resident name_box; L.resident card_label ]
  in
  let rent_box =
    L.superpose ~center:true [ L.resident one_property; L.resident rent_label ]
  in
  let _ = L.sety rent_box (-20) in

  let card_details1 =
    L.tower ~align:Center ~sep:100 [ name_layout; rent_info ]
  in
  let card_details = L.tower ~align:Center [ card_details1; rent_box ] in

  let final_card_layout =
    L.superpose ~center:true [ L.resident card; card_details ]
  in

  (* Only add click handler if it's not a popup and is clickable *)
  let final_layout =
    if (not is_popup) && clickable then (
      let card_id = !next_card_id in
      incr next_card_id;

      let clickable_container =
        W.box ~w:400 ~h:628
          ~style:Style.(create ~background:(Style.color_bg (0, 0, 0, 0)) ())
          ()
      in

      let combined_layout =
        L.superpose ~center:true
          [ final_card_layout; L.resident clickable_container ]
      in

      (* Store the info using the card ID and name only *)
      Hashtbl.add card_info card_id (name, combined_layout, bg_color);

      let _ =
        W.on_click
          ~click:(fun _mouse_button ->
            if not !popup_active then begin
              popup_active := true;
              try
                Lwt.wakeup !card_resolver (name, bg_color);
                (* Modified to pass just name and color *)
                print_endline "Successfully resolved promise"
              with Invalid_argument _ ->
                print_endline "Promise already resolved - ignoring click"
            end
            else begin
              print_endline "Click ignored - popup is active"
            end;
            flush stdout)
          clickable_container
      in

      combined_layout)
    else final_card_layout
  in

  final_layout

let create_deck () =
  List.map
    (fun (name, color) ->
      let card =
        create_property_card ~name ~bg_color:color ~is_popup:false
          ~clickable:true
      in
      print_endline ("Created card: " ^ name);
      card)
    properties

let color_of_string = function
  | "Brown" -> (139, 69, 19, 255) (* Saddle brown *)
  | "Light Blue" -> (135, 206, 235, 255) (* Sky blue *)
  | "Pink" -> (255, 182, 193, 255) (* Light pink *)
  | "Orange" -> (255, 165, 0, 255) (* Pure orange *)
  | "Red" -> (255, 0, 0, 255) (* Pure red *)
  | "Yellow" -> (255, 255, 0, 255) (* Pure yellow *)
  | "Green" -> (0, 128, 0, 255) (* Pure green *)
  | "Blue" -> (0, 0, 255, 255) (* Pure blue *)
  | _ -> (128, 128, 128, 255)
(* Default gray for unknown colors *)

let create_overlapping_cards widgets =
  (* Set each card to its fixed size *)
  List.iter (fun widget -> 
    L.set_width widget (int_of_float(400.0 *. 0.8));
    L.set_height widget (int_of_float(628. *. 0.8))
  ) widgets;
  
  let layout = L.flat ~sep:(-280) widgets in
  L.sety layout 0;
  layout

let card_layouts = Hashtbl.create 10  (* color -> layout mapping *)

let create_vertical_cards widgets =
  print_endline
    ("Creating vertical layout with "
    ^ string_of_int (List.length widgets)
    ^ " widgets");
  (* Create a fixed-height container *)
  let container = L.tower ~align:Center ~sep:20 widgets in
  (* Force the layout to stay within bounds *)
  L.set_height container 620;
  container

let create_horizontal_cards widgets =
  print_endline
    ("Creating horizontal layout with "
    ^ string_of_int (List.length widgets)
    ^ " widgets");
  (* Scale each widget appropriately *)
  List.iter
    (fun widget ->
      L.set_width widget (int_of_float (400.0 *. 0.8));
      L.set_height widget (int_of_float (628.0 *. 0.8));
      L.show widget)
    widgets;

  (* Create layout with minimal fixed spacing *)
  let layout = L.flat ~sep:10 ~align:Center widgets in
  (* Force the layout to stay within the container width *)
  let layout_width =
    (int_of_float (400.0 *. 0.8) * List.length widgets)
    + (10 * (List.length widgets - 1))
  in
  L.set_width layout layout_width;
  layout

let organize_table_cards cards =
  print_endline "\n=== organize_table_cards start ===";
  print_endline ("Total cards to organize: " ^ string_of_int (List.length cards));

  (* Group cards by color *)
  let color_groups = Hashtbl.create 10 in

  List.iter
    (fun card ->
      let card_color =
        Hashtbl.fold
          (fun _id (name, layout, bg_color) color ->
            if layout == card then
              let _ = print_endline ("Found card: " ^ name ^ " with color") in
              Some bg_color
            else color)
          card_info None
      in

      match card_color with
      | Some color ->
          let group =
            try Hashtbl.find color_groups color with Not_found -> []
          in
          print_endline "Adding card to color group";
          Hashtbl.replace color_groups color (card :: group)
      | None -> print_endline "Warning: Card color not found!")
    cards;

  (* Create layouts for each color group *)
  let group_layouts =
    Hashtbl.fold
      (fun _ cards acc ->
        print_endline
          ("Creating group with " ^ string_of_int (List.length cards) ^ " cards");
        let layout =
          match List.rev cards with
          | [] -> None
          | [ single ] ->
              print_endline "Creating single card vertical layout";
              Some (create_vertical_cards [ single ])
          | multiple ->
              print_endline "Creating multiple card horizontal layout";
              Some (create_horizontal_cards multiple)
        in
        match layout with
        | Some l -> l :: acc
        | None -> acc)
      color_groups []
  in

  (* Create final vertical layout *)
  let final_layout =
    match group_layouts with
    | [] ->
        let spacer =
          W.box ~w:1 ~h:800
            ~style:Style.(create ~background:(color_bg (0, 0, 0, 0)) ())
            ()
        in
        L.resident spacer
    | layouts -> L.tower ~sep:20 ~align:Center layouts
  in

  print_endline
    ("Final layout created with "
    ^ string_of_int (List.length group_layouts)
    ^ " groups");
  final_layout
