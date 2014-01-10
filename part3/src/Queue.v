Require Import Arith List.

Extraction Language Haskell.
Extract Inductive bool => "Prelude.Bool" ["Prelude.True" "Prelude.False"].
Extract Inductive nat => "Prelude.Int" ["0" "Prelude.succ"]
  "(\fO fS n -> case n of {
      0 -> fO ();
      n -> fS ((Prelude.-) n 1) })".

Extract Inductive option => "Prelude.Maybe" ["Prelude.Just" "Prelude.Nothing"].
Extract Inductive list => "[]" ["[]" "(:)"].
Extract Inductive prod => "(,)" ["(,)"].
Extract Constant app => "(Prelude.++)".
Extract Constant rev => "Prelude.reverse".
Extract Constant andb => "(Prelude.&&)".
Extract Constant negb => "Prelude.not".
Extract Constant leb => "(Prelude.<=)".
Extract Constant length => "Prelude.length".
Extract Constant beq_nat => "(Prelude.==)".
Extract Constant id => "Prelude.id".
Extract Constant plus => "(Prelude.+)".
Extract Constant minus => "(Prelude.-)".

Module Type QUEUE.
  Parameter Q : Type -> Type.
  Parameter inv : forall A, Q A -> Prop.
  Parameter empty  : forall A, Q A.
  Parameter inject : forall A, A -> Q A -> Q A.
  Parameter pop    : forall A, Q A -> Q A.
  Parameter peak   : forall A, Q A -> option A.  
  Parameter to_list : forall A, Q A -> list A.
  
  Definition represents {A : Type} (q : Q A) (l : list A) : Prop :=
    to_list _ q = l.

  Axiom empty_inv :
    forall A, 
      inv A (empty A).

  Axiom inject_inv :
    forall A q x,
      inv A q ->
      inv _ (inject _ x q).

  Axiom pop_inv :
    forall A q,
      inv A q ->
      inv _ (pop _ q).
  
  Axiom empty_is_empty : forall A,
    represents (empty A) nil.
  
  Axiom inject_spec :
    forall A l q x,
      represents q l ->
      inv _ q ->
      represents (inject A x q) (l ++ (x :: nil)).
  
  Axiom pop_empty :
    forall A q,
      represents q nil ->
      inv _ q ->
      represents (pop A q) nil.

  Axiom pop_xs :
    forall A q x xs,
      represents q (x::xs) ->
      inv _ q ->
      represents (pop A q) xs.
  
  Axiom peak_empty_spec :
    forall A q,
      represents q nil ->
      inv _ q -> 
      peak A q = None.

  Axiom peak_spec :
    forall A q x xs,
      represents q (x::xs) ->
      inv _ q ->
      peak A q = Some x.

End QUEUE.

Module BasicQueue <: QUEUE.

  Definition Q := list.

  Definition inv := fun (A : Type) (q : Q A) => True.

  Definition empty := @nil.

  Fixpoint inject {A : Type} (a : A) (q : Q A) : Q A :=
    match q with
      | nil => a :: nil
      | x :: q' => x :: inject a q'
    end.

  Lemma unfold_inject_base : 
    forall A (a : A),
      inject a nil = a :: nil.
  Proof. reflexivity. Qed.

  Lemma unfold_inject_step :
    forall A (a : A) x q,
      inject a (x :: q) = x :: inject a q.
  Proof. reflexivity. Qed.

  Definition pop {A : Type} (q : Q A) :=
    match q with
      | nil => nil
      | _ :: q' => q'
    end.

  Definition peak {A : Type} (q : Q A) :=
    match q with
      | nil => None
      | x :: _ => Some x
    end.

  Definition to_list {A : Type} (q : Q A) := q.
  
  Definition represents {A : Type} (q : Q A) (l : list A) : Prop :=
    to_list q = l.

  Lemma empty_inv : 
    forall A,
      inv A (empty A).
  Proof. constructor. Qed.

  Lemma inject_inv :
    forall A q x,
      inv A q ->
      inv _ (inject x q).
  Proof. constructor. Qed.

  Lemma pop_inv : 
    forall A q,
      inv A q ->
      inv _ (pop q).
  Proof. constructor. Qed.
 
  Lemma empty_is_empty : 
    forall A,
      represents (empty A) nil.
  Proof.
    unfold represents.
    unfold to_list.
    unfold empty.
    reflexivity.
  Qed.

  Lemma inject_spec :
    forall A l q (x : A),
      represents q l ->
      inv _ q ->
      represents (inject x q) (l ++ (x :: nil)).
  Proof.
    unfold represents, to_list.
    intros a l.
    induction l.
    intros.
    rewrite H.
    reflexivity.
    intros.
    rewrite H.
    rewrite unfold_inject_step.
    rewrite (IHl l x).
    rewrite app_comm_cons.
    reflexivity.
    reflexivity.
    constructor.
  Qed.

  Lemma pop_empty :
    forall A (q : Q A),
      represents q nil ->
      inv _ q ->
      represents (pop q) nil.
  Proof.
    unfold represents, to_list.
    intros.
    rewrite H.
    reflexivity.
  Qed.

  Lemma pop_xs :
    forall A (q : Q A) x xs,
      represents q (x::xs) ->      
      inv _ q ->
      represents (pop q) xs.
  Proof.
    unfold represents, to_list.
    intros.
    rewrite H.
    unfold pop.
    reflexivity.
  Qed.
  
  Lemma peak_empty_spec :
    forall A (q : Q A),
      represents q nil ->
      inv _ q ->
      peak q = None.
  Proof.
    unfold represents, to_list.
    intros.
    rewrite H.
    reflexivity.
  Qed.

  Lemma peak_spec :
    forall A (q : Q A) x xs,
      represents q (x::xs) ->
      inv _ q -> 
      peak q = Some x.
  Proof.
    unfold represents, to_list.
    intros.
    rewrite H.
    unfold peak.
    reflexivity.
  Qed.

End BasicQueue.

Extraction "BasicQueue.hs" BasicQueue.

Module PairQueue <: QUEUE.

  Definition Q := fun A : Type => ((list A) * (list A))%type.

  Definition inv := fun (A : Type) (q : Q A) => True.

  Definition empty {A : Type} := (@nil A, @nil A).

  Definition inject {A : Type} (a : A) (q : Q A) := 
    match q with
      | (h, t) => (h, a :: t)
    end.

  Definition pop {A : Type} (q : Q A) :=
    match q with
      | (h, t) => 
        match h with
          | (_ :: h') => (h', t)
          | nil =>
            match rev t with
              | nil => (nil, nil)
              | (_ :: t') => (t', nil)
            end
        end
    end.

  Definition peak {A : Type} (q : Q A) : option A :=
    match q with
      | (h, t) =>
        match h with
          | (x :: _) => Some x
          | nil =>
            match rev t with
              | nil => None
              | (x :: _) => Some x
            end
        end
    end.

  Definition to_list {A : Type} (q : Q A) :=
    match q with
      | (h, t) => h ++ rev t
    end.

  Definition represents {A : Type} (q : Q A) (l : list A) : Prop :=
    to_list q = l.

  Lemma empty_inv : 
    forall A,
      inv A (@empty A).
  Proof. constructor. Qed.

  Lemma inject_inv :
    forall A q x,
      inv A q ->
      inv _ (inject x q).
  Proof. constructor. Qed.

  Lemma pop_inv : 
    forall A q,
      inv A q ->
      inv _ (pop q).
  Proof. constructor. Qed.

  Lemma empty_is_empty : forall A,
    represents (@empty A) nil.
  Proof.
    unfold represents.
    unfold to_list.
    unfold empty.
    reflexivity.
  Qed.

  Lemma inject_spec :
    forall A l q (x : A),
      represents q l ->
      inv _ q -> 
      represents (inject x q) (l ++ (x :: nil)).
  Proof.
    unfold represents, to_list. 
    intros.
    destruct q as (h, t).
    unfold inject.
    simpl.
    rewrite app_assoc.
    rewrite H.
    reflexivity.
  Qed.

  Lemma pop_empty :
    forall A (q : Q A),
      represents q nil ->
      inv _ q ->
      represents (pop q) nil.
  Proof.
    unfold represents, to_list, pop.
    intros.
    destruct q as (h, t).
    destruct h.
    destruct (rev t).
    reflexivity.
    inversion H.        
    inversion H.
  Qed.      
  
  Lemma pop_xs :
    forall A (q : Q A) x xs,
      represents q (x::xs) ->
      inv _ q -> 
      represents (pop q) xs.
  Proof.
    unfold represents, to_list.
    intros.
    destruct q as (h, t).
    unfold pop.
    
    destruct h.
    destruct (rev t).
    inversion H.
    inversion H.
    simpl.
    rewrite app_nil_r.
    reflexivity.
    inversion H.
    reflexivity.    
  Qed.
  
  Lemma peak_empty_spec :
    forall A (q : Q A),
      represents q nil ->
      inv _ q -> 
      peak q = None.
  Proof.
    unfold represents, to_list.
    intros.
    destruct q as (h, t).
    apply app_eq_nil in H.
    destruct H as [Hh Ht].
    unfold peak.
    rewrite Hh, Ht.
    reflexivity.
  Qed.

  Lemma peak_spec :
    forall A (q : Q A) x xs,
      represents q (x::xs) ->
      inv _ q ->
      peak q = Some x.
  Proof.
    unfold represents, to_list.
    destruct q as (h, t).
    intros.
    destruct h.
    simpl in H.
    unfold peak.
    rewrite H.
    reflexivity.
    unfold peak.
    inversion H.
    reflexivity.
  Qed.  
End PairQueue.

Extraction "PairQueue.hs" PairQueue.

Lemma length_O_is_nil :
  forall A xs,
    length xs = 0 ->
    xs = @nil A.
Proof.
  induction xs; intros.
  reflexivity.
  inversion H.
Qed.

Module OkasakiQueue <: QUEUE.

  Definition Q (A : Type) :=
    (list A * list A)%type.
  
  Definition inv (A : Type) (q : Q A) : Prop :=
    let (h, t) := q in
    leb (length t) (length h) = true.

  Definition empty {A : Type} := 
    (@nil A, @nil A).

  Definition ltb n m : bool :=
    leb (S n) m.

  Definition inject {A : Type} (a : A) (q : Q A) :=
    let (h, t) := q in
    if (ltb (length t) (length h))
    then (h, a :: t)
    else (h ++ rev (a :: t), nil).

  Definition pop {A : Type} (q : Q A) : Q A :=
    let (h, t) := q in
    match h with
      | nil => (nil, nil)
      | (e :: h') => 
        if beq_nat (length h) (length t)
        then (h' ++ rev t, nil)
        else (h', t)
    end.
 
  Definition peak {A : Type} (q : Q A) : option A :=
    let (h, t) := q in
    match h with
      | nil => None
      | e :: _ => Some e
    end.

  Definition to_list {A : Type} (q : Q A) :=
    let (h, t) := q in
    h ++ rev t.
  
  Definition represents {A : Type} (q : Q A) (l : list A) : Prop :=
    to_list q = l.

  Lemma empty_inv :
    forall A, 
      inv A (@empty A).
  Proof.
    intro A.
    unfold inv.
    unfold empty.
    simpl.
    reflexivity.
  Qed.
      
  Lemma unfold_length :
    forall {A : Type} (e : A) (l : list A),
      length (e::l) = S (length l).
  Proof.
    auto.
  Qed.
  
  Lemma inject_inv :
    forall A q x,
      inv A q ->
      inv _ (inject x q).
  Proof.
    unfold inv, inject.
    intros A [h t] x Hq.
    remember (ltb _ _).
    destruct b.
      symmetry in Heqb.
      unfold ltb in Heqb.
      rewrite unfold_length.
      assumption.

      unfold length at 1.
      reflexivity.
  Qed.

  Lemma pop_inv :
    forall A q,
      inv A q ->
      inv _ (pop q).
  Proof.
    unfold inv, pop.
    destruct q as (h, t).
    intros.
    destruct h.
    reflexivity.
    remember (beq_nat (length (a :: h)) (length t)) as hvt.
    destruct hvt.
    reflexivity.
    apply leb_correct.
    apply leb_complete in H.
    simpl in H.
    symmetry in Heqhvt.
    apply beq_nat_false in Heqhvt.
    simpl in Heqhvt.
    apply NPeano.Nat.le_succ_r in H.
    destruct H.
    assumption.
    symmetry in H.
    contradiction.
  Qed.

  Lemma empty_is_empty : forall A,
    represents (@empty A) nil.
  Proof. reflexivity. Qed.
  
  Lemma inject_spec :
    forall A l q x,
      represents q l ->
      inv A q ->
      represents (inject x q) (l ++ (x :: nil)).
  Proof.
    unfold represents, to_list, inject.
    intros A l [h t] x Hrep Hinv.
    case (ltb _ _);
      simpl;
      rewrite<- Hrep;
      try rewrite app_nil_r;
      rewrite app_assoc;
      reflexivity.
  Qed.

  Lemma pop_empty :
    forall A q,
      represents q nil ->
      inv A q ->
      represents (pop q) nil.
  Proof.
    unfold represents, to_list, inv.
    destruct q as (h, t).
    intros.
    apply app_eq_nil in H.
    destruct H as [Hh Ht].
    unfold pop.
    rewrite Hh.
    reflexivity.
  Qed.

  Lemma pop_xs :
    forall A q x xs,
      represents q (x::xs) ->
      inv A q ->
      represents (pop q) xs.
  Proof.
    unfold represents, to_list, inv.
    destruct q as (h, t).
    intros.
    unfold pop.
    destruct h.
    simpl in H, H0.
    apply leb_complete in H0.
    inversion H0.
    apply length_O_is_nil in H2.
    rewrite H2 in H.
    inversion H.
    inversion H.
    destruct (beq_nat (length (x :: h)) (length t)).
    rewrite app_nil_r.
    reflexivity.
    reflexivity.
  Qed.

  Lemma peak_empty_spec :
    forall A q,
      represents q nil ->
      inv A q -> 
      peak q = None.
  Proof.
    unfold represents, to_list, inv, peak.
    destruct q as (h, t).
    intros.
    destruct h.
    reflexivity.
    inversion H.
  Qed.

  Lemma peak_spec :
    forall A q x xs,
      represents q (x::xs) ->
      inv A q ->
      peak q = Some x.
  Proof.
    unfold represents, to_list, inv, peak.
    destruct q as (h, t).
    intros.
    destruct h.
    simpl in H0.
    apply leb_complete in H0.
    inversion H0.
    apply length_O_is_nil in H2.
    rewrite H2 in H.
    inversion H.
    inversion H.
    reflexivity.
  Qed.
End OkasakiQueue.

Extraction "OkasakiQueue.hs" OkasakiQueue.

Module RealTimeQueue <: QUEUE.
  
  Inductive RotationState {A : Type} :=
  | Idle : RotationState
  | Reversing : nat -> list A -> list A -> list A -> list A -> RotationState 
  | Appending : nat -> list A -> list A -> RotationState 
  | Done : list A -> RotationState.
  
  Definition exec {A : Type} (s : @RotationState A) :=
    match s with
      | Reversing ok (x :: f) f' (y :: r) r' => 
        Reversing (S ok) f (x :: f') r (y :: r')
      | Reversing ok nil f' (y :: nil) r' =>
        Appending ok f' (y :: r')
      | Appending 0 f' r' =>
        Done r'
      | Appending (S ok) (x :: f') r' =>
        Appending ok f' (x :: r')
      | _ => s
    end.

  Fixpoint nth {A : Type} n (f : A -> A) : A -> A:=
    match n with
      | O => id
      | S n' => fun x => f ((nth n' f) x)
    end.

  Definition Q (A : Type) :=
    (nat * list A * nat * list A * @RotationState A)%type.
                     
  Definition invalidate {A : Type} (s : @RotationState A) :=
    match s with
      | Reversing (S ok) f f' r r' =>
        Reversing (ok ) f f' r r'
      | Appending 0 f' (x :: r') =>
        Done r'
      | Appending (S ok) f' r' =>
        Appending ok f' r'
     | _ => s
    end.
  
  Definition exec2 {A : Type} (inp : Q A) := 
    match inp with 
      | (lenf, f, lenr, r, state) =>
        match exec (exec state) with
          | Done newf => (lenf, newf, lenr, r, Idle)
          | newstate => (lenf, f, lenr, r, newstate)
        end
    end.

  Definition check {A : Type} (inp : Q A) :=
    match inp with
      | (lenf, f, lenr, r, state) =>
        if leb lenr lenf 
        then exec2 inp
        else let newstate := Reversing 0 f nil r nil in
             exec2 (lenf + lenr, f, 0, nil, newstate)
    end.

  Definition ltb n m : bool :=
    leb (S n) m.

  Definition inv_state {A : Type} (q : @Q A) : Prop :=
    match q with
      | (lenf, f, _, _, Idle)      => 
        lenf = length f
      | (lenf, _, _, _, Done newf) => 
        False
      | (lenf, _, _, _, Reversing ok f f' r r') => 
        (length f' = length r') /\ 
        (S (length f) = length r) /\
        leb ok (length f') = true /\
        ltb 1 ok = true /\
        lenf = ok + length f + length r + length r'
      | (lenf, _, _, _, Appending ok f' r') => 
        ltb (length f') (length r') = true /\
        leb ok (length f') = true /\
        lenf = ok + length r'
    end.
    
  Definition idling {A : Type} (s : @RotationState A) : Prop :=
    match s with
      | Idle => True
      | _ => False
    end.

  Definition inv {A : Type} (q : Q A) : Prop := 
    match q with 
      | (lenf, f, lenr, r, state) =>
        leb lenr lenf = true /\ 
        lenr = length r /\ 
        inv_state q /\
        (not (idling state) -> leb (S lenr) lenf = true) /\ 
        (not (idling state) -> not(length f = 0))   
    end.

  Definition empty {A : Type} : Q A :=
    (0, @nil A, 0, @nil A, @Idle A).

  Definition inject {A : Type} (a : A) (q : Q A) :=
    match q with
      | (lenf, f, lenr, r, state) =>
        check (lenf, f, S lenr, a :: r, state)
    end.

  Definition pop {A : Type} (q : Q A) :=
    match q with
      | (lenf, nil, lenr, r, state) => 
        q
      | (lenf, x :: f, lenr, r, state) =>
        check (lenf - 1, f, lenr, r, invalidate state)
    end.

  Definition peak {A : Type} (q : Q A) :=
    match q with
      | (lenf, nil, lenr, r, state) => 
        None
      | (lenf, x :: f, lenr, r, state) =>
        Some x
    end.

  Compute (inject 1 (inject 2 (inject 3 empty))).

  Fixpoint take {A : Type} n (xs : list A) :=
    match n with
      | O => nil
      | S n' =>
        match xs with
          | nil => nil
          | x::xs' =>  x :: (take n' xs')
        end
    end.

  Lemma rev_nil :
    forall A,
      rev nil = (nil : list A).
  Proof.
    auto.
  Qed.

  Lemma rev_cons :
    forall A xs (x : A),
      rev (x::xs) = rev xs ++ x :: nil.
  Proof.
    auto.
  Qed.

  Lemma rev_eq_nil :
    forall A (l : list A),
      rev l = nil -> l = nil.
  Proof.
    intros.
    destruct l; auto.
    rewrite rev_cons in H.
    apply app_eq_nil in H.
    destruct H as [_ Habsurd].
    inversion Habsurd.
  Qed.

  Lemma length_cons :
    forall A xs (x:A),
      length (x::xs) = S (length xs).
  Proof.
    auto.
  Qed.

  Lemma n_eq_m :
    forall n (m : nat),
      n = m <-> (S n) = (S m).
    split; intro H.
    rewrite H; reflexivity.
    apply eq_nat_is_eq in H.
    simpl in H.
    apply eq_nat_is_eq in H.
    assumption.
  Qed.
    
  Lemma leb_Sn_Sm :
    forall n m b,
      (leb (S n) (S m) = b) <-> leb n m = b.
  Proof.
    intros.
    split.
    intro h.
    auto.
    intro h.
    auto.
  Qed.

  Lemma leb_Sn_n :
    forall m n,
      leb (S n) m = true -> leb n m = true.
  Proof.
    intros m n H.
    apply leb_iff.
    apply leb_iff in H.
    apply le_Sn_le.
    assumption.
  Qed.

  Lemma leb_m_Sm :
    forall m n,
      leb n m = true -> leb n (S m) = true.
  Proof.
    intros m n H.
    destruct n.
    auto.
    destruct m.
    inversion H.
    apply leb_Sn_n.
    apply leb_Sn_Sm.
    assumption.
  Qed.

  Lemma leb_n_n :
    forall n,
      leb n n = true.
  Proof.
    intro n.
    induction n; auto.
  Qed.

  Lemma leb_n_Sn :
    forall n,
      leb n (S n) = true.
  Proof.
    intro n.
    induction n; auto.
  Qed.

  Lemma leb_n_SSn :
    forall n,
      leb n (S (S n)) = true.
  Proof.
    intro n.
    induction n; auto.
  Qed.

  Lemma take_cons :
    forall A n (x:A) xs,
      take (S n) (x::xs) = x :: (take n xs).
  Proof.
    auto.
  Qed.

  Lemma take_nil :
    forall (A : Type) n,
      take n nil = (@nil A).
  Proof.
    intros.
    destruct n;
      auto.
  Qed.
  
  Lemma take_O :
    forall A (l : list A),
      take O l = nil.
  Proof.
    auto.
  Qed.

  Lemma take_rev_cons :
    forall A n l,
      leb (S n) (length l) = true ->
      exists (x:A), 
        rev (take (S n) l) = x :: rev (take n l).
  Proof.
    intros A n. 
    induction n.
      intros [ | hd l'] Hleb.
        simpl in Hleb.
        discriminate Hleb.
        
        exists hd.
        simpl.
        reflexivity.

      intros [ | hd l'] Hleb.
        simpl in Hleb.
        discriminate Hleb.

        rewrite ? take_cons.
        rewrite ? rev_cons.
        rewrite ? length_cons in Hleb.
        apply leb_Sn_Sm in Hleb.
        specialize (IHn l' Hleb).
        destruct IHn.
        exists x.
        rewrite app_comm_cons.
        rewrite H.
        reflexivity.
  Qed.

  Definition to_list {A : Type} (q : Q A) :=
    match q with
      | (lenf, front, lenr, rear, state) =>
        match state with
          | Appending 0 _ r' =>
            r'
          | Appending ok f' r' => 
            rev (take ok f') ++ r'
          | Reversing ok f f' r r' =>
            rev (take ok f') ++ f ++ rev r ++ r'
          | Done f => f
          | Idle => front
        end
        ++ rev rear
    end.
  
  Definition represents {A : Type} (q : Q A) (l : list A) : Prop :=
    @to_list _ q = l.

  Lemma check_doesnt_change_invariant :
    forall A lenf (f : list A) lenr r state,
      inv (lenf, f, lenr, r, state) ->
      inv (check (lenf, f, lenr, r, state)).
  Proof.
    assert (Hfalse : False -> False); auto.
    intros A lenf f lenr r state Hinv.
    assert (Hinv' : inv (lenf, f, lenr, r, state)). exact Hinv.
    destruct Hinv as [Hinv1 [Hinv2 [Hinv3 [Hinv4 Hinv5]]]].
    unfold inv_state in Hinv3.
    destruct state as [ | valid front front' rear rear'| valid front' rear' | front].
    
      unfold check.
      rewrite Hinv1.
      unfold exec2.
      unfold exec.
      assumption.

      destruct Hinv3 as [Hinv3a [Hinv3b [Hinv3c [Hinv3d Hinv3e]]]].
      specialize (Hinv4 Hfalse).
      specialize (Hinv5 Hfalse).
      unfold check.
      rewrite Hinv1.
      unfold exec2.
      unfold exec at 2.
      destruct front.
      destruct rear.
      unfold exec.
      assumption.
      destruct rear.
      unfold exec.
      destruct valid.
      inversion Hinv3d.
      destruct front'.
      inversion Hinv3c.
      destruct valid.
      inversion Hinv3d.
      destruct front'.
      unfold length in Hinv3c.
      inversion Hinv3c.
      unfold inv; repeat split; try intro H; try assumption.
      unfold ltb.
      rewrite ? length_cons.
      rewrite <- Hinv3a.
      rewrite ? length_cons.
      apply leb_Sn_Sm.
      apply leb_Sn_Sm.
      apply leb_n_SSn.
      unfold exec.
      unfold inv; repeat split; try intro H; try assumption.
      rewrite Hinv3e.
      rewrite ? length_cons.
      simpl.
      ring.
      destruct rear.
      unfold exec.
      unfold inv; repeat split; try intro H; try assumption.
      unfold exec.
      unfold inv; repeat split; try intro H; try assumption.
      destruct rear.
      unfold exec.
      unfold inv; repeat split; try intro H; try assumption.
      unfold exec.
      destruct front.
      destruct rear.
      inversion Hinv3b.
      destruct rear.
      unfold inv; repeat split; try intro H; try assumption.
      rewrite ? length_cons.
      rewrite Hinv3a.
      apply leb_Sn_Sm.
      apply leb_Sn_Sm.
      apply leb_n_n.
      rewrite Hinv3e.
      rewrite ? length_cons.
      simpl.
      ring.
      inversion Hinv3b.
      destruct rear.
      inversion Hinv3b.
      unfold inv; repeat split; try intro H; try assumption.
      simpl.
      rewrite Hinv3a.
      reflexivity.
      rewrite ? length_cons in Hinv3b.
      apply <- n_eq_m in Hinv3b.
      apply <- n_eq_m in Hinv3b.
      exact Hinv3b.
      rewrite Hinv3e.
      rewrite ? length_cons.
      simpl.
      ring.
      
      destruct Hinv3 as [Hinv3a [Hinv3b Hinv3c]].
      specialize (Hinv4 Hfalse).
      specialize (Hinv5 Hfalse).
      unfold check.
      rewrite Hinv1.
      unfold exec2.
      unfold exec.
      destruct valid.
      unfold inv; repeat split; try intro H; try assumption.
      unfold idling, not in H.
      specialize (H I).
      inversion H.
      destruct front'.
      unfold inv; repeat split; try intro H; try assumption.
      destruct valid.
      unfold inv; repeat split; try intro H; try assumption.
      unfold idling, not in H.
      specialize (H I).
      inversion H.
      destruct front'.
      unfold length in Hinv3b.
      apply -> leb_Sn_Sm in Hinv3b.
      inversion Hinv3b.
      unfold inv; repeat split; try intro H; try assumption.

      rewrite 2 length_cons in Hinv3a.
      rewrite 2 length_cons.
      unfold ltb.
      unfold ltb in Hinv3a.
      apply leb_Sn_n in Hinv3a.
      apply leb_Sn_n in Hinv3a.
      apply leb_m_Sm in Hinv3a.
      apply leb_m_Sm in Hinv3a.
      assumption.
      rewrite Hinv3c.
      rewrite 2 length_cons.
      ring.

      inversion Hinv3.
  Qed.

  Lemma exec_doesnt_change_representation :
    forall A lenf (f : list A) lenr r state l,
      represents (lenf, f, lenr, r, state) l ->
      represents (lenf, f, lenr, r, (exec state)) l.
  Proof.
    intros A lenf f lenr r state l Hrep.
    unfold represents, to_list in Hrep.
    destruct state as [ | valid front front' rear rear'| valid front' rear' | front].
    
      unfold exec.
      assumption.

      unfold exec, represents, to_list.
      destruct front.
      destruct rear.
      destruct valid;
      assumption.
      destruct rear.
      destruct valid;
      assumption.
      assumption.
      destruct rear.
      assumption.
      rewrite <- Hrep.
      simpl.
      rewrite <-? app_assoc.
      rewrite <-? (app_comm_cons nil).
      rewrite ? app_nil_l.
      rewrite <- (app_comm_cons _ _ a).
      rewrite ? app_assoc.
      reflexivity.
      unfold exec.
      unfold represents; unfold to_list.
      rewrite <- Hrep.
      destruct valid.
      reflexivity.
      destruct front'.
      reflexivity.
      destruct valid.
      reflexivity.
      simpl.
      remember (match front' with
                  | nil => nil
                  | x :: xs' => x :: take valid xs'
                end).
      rewrite <-? app_assoc.
      rewrite <-? (app_comm_cons nil).
      rewrite app_nil_l.
      rewrite app_comm_cons.
      reflexivity.
      
      unfold exec.
      rewrite <- Hrep.
      reflexivity.
  Qed.

  Lemma exec2_doesnt_change_representation :
    forall A lenf (f : list A) lenr r state l,
      represents (lenf, f, lenr, r, state) l ->
      represents (exec2 (lenf, f, lenr, r, state)) l.
  Proof.
    unfold exec2.
    intros.
    case_eq (exec (exec state));
    intros;
    apply exec_doesnt_change_representation in H;
    apply exec_doesnt_change_representation in H;
    rewrite H0 in H;
    unfold represents, to_list in H;
    unfold represents, to_list;
    assumption.      
  Qed.
  
  Lemma empty_inv :
    forall A, 
      inv (@empty A). 
  Proof.
    intro A.
    unfold inv.
    split.
    auto.
    split.
    auto.
    split.
    unfold inv_state.
    simpl.
    auto.
    split;
      simpl;
      unfold not;
      intro;
      assert (Ht : True);
      auto;
      specialize (H Ht);
      inversion H.
  Qed.
          
  Lemma inject_inv :
    forall A (q : Q A) x,
      inv q ->
      inv (inject x q).
  Proof.
    assert (Hfalse : False -> False). auto.
    intros A q x Hinv.
    destruct q as [[[[lenf f] lenr] r] state].
    assert (Hinv' : inv (lenf, f, lenr, r, state)). exact Hinv.
    destruct Hinv as [Hinv1 [Hinv2 [Hinv3 [Hinv4 Hinv5]]]].
    unfold inv_state in Hinv3.
    unfold inject.
    apply check_doesnt_change_invariant.
    destruct state as [ | valid front front' rear rear'| valid front' rear' | front].

      admit.
      
      destruct Hinv3 as [Hinv3a [Hinv3b [Hinv3c [Hinv3d Hinv3e]]]].
      specialize (Hinv5 Hfalse).
      specialize (Hinv4 Hfalse).
      unfold inv; repeat split; try intro H; try assumption.
      admit.
      admit.
      

      destruct Hinv3 as [Hinv3a [Hinv3b Hinv3c]].
      specialize (Hinv5 Hfalse).
      specialize (Hinv4 Hfalse).
      unfold inv; repeat split; try intro H; try assumption.
      admit.
      admit.
      
      inversion Hinv3.
  Qed.    

  Lemma pop_inv :
    forall A (q : Q A),
      inv q ->
      inv (pop q).
  Proof. 
Admitted.
    
  Lemma empty_is_empty : 
    forall A,
      represents (@empty A) nil.
  Proof.
    intro A.
    unfold represents.
    unfold to_list.
    simpl.
    reflexivity.
  Qed.

 Lemma inject_spec :
    forall A l (q : Q A) x,
      represents q l ->
      inv q ->
      represents (inject x q) (l ++ (x :: nil)).
  Proof. 
    assert (H: forall A (x:A) l, rev (x::l) = rev l ++ x :: nil); auto.
    intros A l [[[[lenf f] lenr] r] state] x.
    intros Hrep Hinv.
    destruct Hinv as [Hinv1 [Hinv2 [Hinv3 [Hinv4 Hinv5]]]].
    unfold inject.
    unfold check.
    unfold represents in Hrep; unfold to_list in Hrep.
    destruct state as [ | valid front front' rear rear'| valid front' rear' | front].
    remember (leb (S _) _).
    destruct b.
    apply exec2_doesnt_change_representation.
    rewrite <- Hrep.
    unfold represents; unfold to_list.
    rewrite H.
    rewrite app_assoc.
    reflexivity.

    apply exec2_doesnt_change_representation.
    rewrite <- Hrep.
    unfold represents; unfold to_list.
    rewrite H.
    simpl.
    rewrite ? app_nil_r.
    rewrite app_assoc.
    reflexivity.

    rewrite Hinv4.
    apply exec2_doesnt_change_representation.
    rewrite <- Hrep.
    unfold represents; unfold to_list.
    rewrite H.
    simpl.
    rewrite app_assoc.
    reflexivity.
    auto.

    rewrite Hinv4.
    apply exec2_doesnt_change_representation.
    rewrite <- Hrep.
    unfold represents; unfold to_list.
    rewrite H.
    rewrite app_assoc.
    reflexivity.
    auto.
    
    rewrite Hinv4.
    apply exec2_doesnt_change_representation.
    rewrite <- Hrep.
    unfold represents; unfold to_list.
    rewrite H.
    rewrite app_assoc.
    reflexivity.
    auto.
  Qed.


  Lemma represents_nil :
    forall A lenf (f : list A) lenr r state,
        represents (lenf, f, lenr, r, state) nil ->
        inv (lenf, f, lenr, r, state) ->
        f = nil.
  Proof.
    assert (Hfalse: False -> False). auto.
    intros A lenf f lenr r state Hrep Hinv.
    destruct Hinv as [Hinv1 [Hinv2 [Hinv3 [Hinv4 Hinv5]]]].
    unfold represents, to_list in Hrep.
    unfold inv_state in Hinv3.
    destruct state as [ | valid front front' rear rear'| valid front' rear' | front].

    apply app_eq_nil in Hrep.
    destruct Hrep as [Hf _].
    assumption.
    
    destruct Hinv3 as [Hinv3a [Hinv3b [Hinv3c Hinv3d]]].
    specialize (Hinv4 Hfalse).
    specialize (Hinv5 Hfalse).
    destruct rear.
    simpl in Hinv3b.
    inversion Hinv3b.
    rewrite rev_cons in Hrep.
    rewrite <-? app_assoc in Hrep.
    remember (rear' ++ rev r) as tl.
    rewrite ->? app_assoc in Hrep.
    remember ((rev (take valid front') ++ front) ++ rev rear) as hd.
    rewrite <- app_assoc in Hrep.
    rewrite <- app_comm_cons in Hrep.
    rewrite app_nil_l in Hrep.
    apply app_eq_nil in Hrep.
    destruct Hrep as [_ Habsurd].
    inversion Habsurd.

    destruct Hinv3 as [Hinv3a [Hinv3b Hinv3c]].
    specialize (Hinv4 Hfalse).
    specialize (Hinv5 Hfalse).
    destruct valid.
    apply app_eq_nil in Hrep.
    destruct Hrep as [Hrear Hr].
    unfold plus in Hinv3c.
    rewrite Hinv3c in Hinv4.
    destruct rear'.
    unfold length in Hinv4.
    inversion Hinv4.
    inversion Hrear.
    destruct front'.
    inversion Hinv3b.
    rewrite take_cons in Hrep.
    rewrite rev_cons in Hrep.
    rewrite <-? app_assoc in Hrep.
    remember (rear' ++ rev r) as tl.
    remember (rev (take valid front')) as hd.
    rewrite <- app_comm_cons in Hrep.
    rewrite app_nil_l in Hrep.
    apply app_eq_nil in Hrep.
    destruct Hrep as [_ Habsurd].
    inversion Habsurd.
    
    inversion Hinv3.
  Qed.
    
  Lemma pop_empty :
    forall A (q : Q A),
      represents q nil ->
      inv q ->
      represents (pop q) nil.
  Proof.
    intros A [[[[lenf f] lenr] r] state].
    intros Hrep Hinv.

    assert (Hf : f = nil).
    apply (represents_nil A lenf f lenr r state).
    assumption.
    assumption.
    unfold pop.
    subst.
    assumption.
  Qed.

  Lemma pop_xs :
    forall A (q : Q A) x xs,
      represents q (x::xs) ->
      inv q ->
      represents (pop q) xs.
  Proof. 
    assert (Hfalse : False -> False). auto.

    assert (Hrev: forall A (xs : list A), rev xs = nil -> xs = nil).
      intros A' xs Hxs.
      destruct xs.
      reflexivity.
      simpl in Hxs.
      apply app_eq_nil in Hxs.
      destruct Hxs as [Hxs Habsurd].
      inversion Habsurd.

    intros A [[[[lenf f] lenr] r] state] x xs.
    intros Hrep Hinv.
    destruct Hinv as [Hinv1 [Hinv2 [Hinv3 [Hinv4 Hinv5]]]].
    unfold inv_state in Hinv3.

    destruct state as [ | valid front front' rear rear'| valid front' rear' | front].
    
      unfold represents in Hrep; unfold to_list in Hrep.
      unfold pop.
      destruct f.
        destruct r.
         simpl in Hrep.
         inversion Hrep.
         
         subst.
         unfold length in Hinv1.
         unfold leb in Hinv1.
         inversion Hinv1.
         auto.
       
       unfold check.
       remember (leb lenr (lenf - 1)).
       destruct b.
       apply exec2_doesnt_change_representation.
       unfold represents, to_list.
       unfold invalidate.
       rewrite <- app_comm_cons in Hrep.
       inversion Hrep.
       reflexivity.
       apply exec2_doesnt_change_representation.
       unfold represents, to_list.
       unfold take.
       unfold rev at 1 3.
       simpl.
       rewrite ? app_nil_r.
       inversion Hrep.
       reflexivity.

       unfold pop.
       specialize (Hinv4 Hfalse).
       specialize (Hinv5 Hfalse).
       destruct f.

       simpl in Hinv5.
       unfold not in Hinv5.
       assert (0 = 0); auto.
       specialize (Hinv5 H).
       inversion Hinv5.

       unfold check.
       destruct lenf.
       inversion Hinv4.
       remember (S lenf - 1).
       simpl in Heqn.
       rewrite <- minus_n_O in Heqn.
       subst.
       simpl in Hinv4.
       rewrite Hinv4.
       apply exec2_doesnt_change_representation.
       unfold invalidate.
       

       destruct valid.
       destruct Hinv3 as [Hinv3a [Hinv3b [Hinv3c [Hinv3d Hinv3e]]]].
       inversion Hinv3d.
       unfold represents, to_list in Hrep.
       destruct Hinv3 as [Hinv3a [Hinv3b [Hinv3c [Hinv3d Hinv3e]]]].
       destruct (take_rev_cons A valid front' Hinv3c).
       rewrite H in Hrep.
       rewrite <-? app_comm_cons in Hrep.
       rewrite <-? app_assoc in Hrep.
       inversion Hrep.
       unfold represents, to_list.
       rewrite <-? app_assoc.
       reflexivity.
       
       specialize (Hinv4 Hfalse).
       specialize (Hinv5 Hfalse).
       destruct f.
       simpl in Hinv5.
       unfold not in Hinv5.
       assert (0 = 0); auto.
       specialize (Hinv5 H).
       inversion Hinv5.
       clear Hinv5.
       
       destruct Hinv3 as [Hinv3a [Hinv3b Hinv3c]].
       destruct valid.
       destruct rear'.
       simpl in Hinv3c.
       rewrite Hinv3c in Hinv4.
       inversion Hinv4.
       unfold pop.
       unfold invalidate.
       unfold check.
       remember (leb lenr (lenf - 1)).
       symmetry in Heqb.
       apply leb_Sn_Sm in Heqb.
       assert (S (lenf - 1) = lenf).
       destruct lenf.
       inversion Hinv3c.
       simpl.
       rewrite <- minus_n_O.
       reflexivity.
       rewrite H in Heqb.
       rewrite Hinv4 in Heqb.
       rewrite <- Heqb.
       apply exec2_doesnt_change_representation.
       unfold represents, to_list.
       unfold represents, to_list in Hrep.
       rewrite <- app_comm_cons in Hrep.
       inversion Hrep.
       reflexivity.

       unfold pop.
       unfold check.
       destruct lenf.
       inversion Hinv4.
       remember (S lenf - 1).
       simpl in Heqn.
       rewrite <- minus_n_O in Heqn.
       subst.
       simpl in Hinv4.
       rewrite Hinv4.
       apply exec2_doesnt_change_representation.
       unfold invalidate.
       unfold represents, to_list in Hrep.
       destruct (take_rev_cons A valid front' Hinv3b).
       rewrite H in Hrep.
       rewrite <-? app_comm_cons in Hrep.
       rewrite <-? app_assoc in Hrep.
       inversion Hrep.
       unfold represents, to_list.
       destruct valid.
       simpl.
       auto.
       rewrite <-? app_assoc.
       reflexivity.

       inversion Hinv3.
  Qed.
  
  Lemma peak_empty_spec :
    forall A (q : Q A),
      represents q nil ->
      inv q -> 
      peak q = None.
  Proof. Admitted.

  Lemma peak_spec :
    forall A (q : Q A) x xs,
      represents q (x::xs) ->
      inv q ->
      peak q = Some x.
  Proof. Admitted.
End RealTimeQueue.

Extraction "RealTimeQueue.hs" RealTimeQueue.