Require Import Arith.
Require Import List.

Fixpoint rangeAux (init: nat) (length: nat) : list nat :=
  match length with
  | O => nil
  | S n => init :: rangeAux (S init) n
  end.

Definition range (a b: nat) : list nat := match le_gt_dec a b with
                                          | left _ => rangeAux a (S(b-a))
                                          | right _ => nil
                                        end.

Infix "--" := range (at level 60).

Fixpoint takeWhile {A:Set} (prop:list A -> bool) (xs:list A):list A :=
  match xs with
    | nil => nil
    | car :: cdr => 
      match prop xs with
        | true => car :: (takeWhile prop cdr)
        | false => nil
      end
  end.

Definition chop {A: Set} (xs : list A) : list A :=
  takeWhile (fun xs => match xs with
                         | x :: nil => false
                         | _ => true
                       end) xs.
  




(* HaskellのsplitAt関数 - yoshihiro503 *)
Fixpoint split_at {A} n (xs:list A) : list A * list A :=
  match (n, xs) with
  | (O, xs) => (nil, xs)
  | (S m, nil) => (nil, nil)
  | (S m, x :: xs) =>
    match split_at m xs with (* Coqでは忍者マッチができない ToT *)
      | (xs1, xs2) => (x::xs1, xs2)
    end
  end.

(*
   nでsplit_atした場合に戻り値の左側のリストの長さが必ずn以下になる
   - yoshihiro503
*)
Lemma length_split_at : forall {A:Set} n (xs xs1 xs2:list A),
  (xs1,xs2) = split_at n xs -> length xs1 <= n.
Proof.
 induction n.
  intros; injection H; intros _ eq; rewrite eq.
  apply le_refl.

  intro xs; destruct xs; simpl; intros; injection H. 

   intros _ eq; rewrite eq.
   apply lt_le_weak.
   apply lt_O_Sn.

   intros _; generalize (IHn xs); simpl.
   destruct (split_at n xs); simpl.
   intro HH; generalize(HH l l0 (refl_equal _)).
   injection H; intros _ eq; rewrite eq.
   apply le_n_S.
Qed.

(* 長さ0のリストはつまりNil - yoshihiro503 *)
Lemma length_nil : forall {A} (xs:list A),
  length xs = 0 -> xs = nil.
Proof.
 intros A xs; destruct xs; [reflexivity | intro H; discriminate].
Qed.

Lemma decreas_aux : forall {A:Set} n (xs xs1 xs2:list A),
  (xs1,xs2) = split_at n xs -> length xs2 <= length xs.
Proof.
 induction n; intros xs xs1 xs2; [ | destruct xs]; simpl.
  intro H; injection H; intros eq _; rewrite eq.
  apply le_refl.

  intro H; injection H; intros eq _; rewrite eq.
  apply le_refl.  

  generalize (IHn xs).
  destruct (split_at n xs).
simpl; intros.
  injection H0; intros eq _; rewrite eq.
  apply (le_trans _ (length xs) _ (H l _ (refl_equal _)) (le_n_Sn _)).
Qed.

Lemma decrease_split_at : forall {A:Set} n (x:A)(xs xs1 xs2:list A),
  0 < n -> (xs1,xs2) = split_at n (x::xs) -> length xs2 < length (x::xs).
Proof.
 intros A n; case n.
  intros; apply (False_ind _ (lt_n_O _ H)).

  intros m x xs xs1 xs2 _.
  simpl.
  generalize (decreas_aux m xs).
  destruct (split_at m xs); intros.
  apply le_lt_n_Sm.
  injection H0; intros H1 _; rewrite H1.
  apply (H l _ (refl_equal _)).
Qed.

Lemma left_split_at : forall {A:Set} n (xs:list A),
  length xs <= n -> let (xs1, xs2) := split_at n xs in xs1 = xs.
Proof.
 induction n; simpl.
  intros; rewrite length_nil; [ | rewrite (le_n_O_eq _ H)]; reflexivity.

  intro xs; destruct xs; simpl.
   reflexivity.

   intro H; generalize (IHn xs).
   destruct (split_at n xs); simpl.
   intro HH; rewrite HH; [reflexivity| ].
   apply (le_S_n _ _ H).
Qed.

(* Haskellのreplicate関数 - yoshihiro503 *)
(* replicate 3 X = [X;X;X] *)
Fixpoint replicate {A:Set} n (x:A) : list A :=
  match n with
    | O => nil
    | S m => x :: replicate m x
  end.

(* replicate nの長さは必ずn - yoshihiro503 *)
Lemma length_replicate : forall (A:Set) n (x:A),
  length (replicate n x) = n.
Proof.
 induction n; simpl.
  reflexivity.

  intro x; rewrite (IHn x); reflexivity.
Qed.

(* 未解決問題 - yoshihiro503 *)
(* Lemma forall {A:Set} n (xs:list A), split_at n xs = (take n xs, drop n xs). *)

Definition concat {A:Set} (xs:list (list A)) :=
  fold_left (fun store l => l ++ store) xs nil.
Definition concat_map {A B:Set} f xs := concat (@map A (list B) f xs).

Fixpoint delete{A:Set}(eq_dec:forall x y:A, {x=y}+{x<>y})(a:A)(xs:list A) :=
  match xs with
   | nil => nil
   | x::xs =>
     if eq_dec x a then
       delete eq_dec a xs
     else
       x :: delete eq_dec a xs
  end.


Lemma delete_dist: forall {A:Set} eq_dec x (xs ys:list A),
  delete eq_dec x (xs++ys) = delete eq_dec x xs ++ delete eq_dec x ys.
Proof.
 induction xs; intros; simpl; [reflexivity | ].
 destruct (eq_dec a x); rewrite IHxs; reflexivity.
Qed.

Lemma delete_replicate : forall {A:Set} eq_dec n (x:A),
  delete eq_dec x (replicate n x) = nil.
Proof.
 induction n; [reflexivity |]; simpl; intros.
 destruct (eq_dec x x); rewrite IHn; [reflexivity | ].
 elim n0; reflexivity.
Qed.

Lemma delete_notIn : forall {A:Set} eq_dec (x:A) xs, ~ In x xs -> delete eq_dec x xs = xs.
Proof.
 induction xs; [reflexivity |]; simpl; intros.
 destruct (eq_dec a x).
  elim H; left; apply e.

  rewrite IHxs; [reflexivity | ].
  intro; elim H; right; apply H0.
Qed.

Lemma map_id : forall{A:Type} (xs:list A), map (fun x => x) xs = xs.
Proof.
 induction xs; [|simpl; rewrite IHxs]; reflexivity.
Qed.
 
