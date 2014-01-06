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
      | Reversing ok f f' r r' =>
        Reversing (ok - 1) f f' r r'
      | Appending 0 f' (x :: r') =>
        Done r'
      | Appending ok f' r' =>
        Appending (ok - 1) f' r'
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

  Definition inv_state {A : Type} (s : @RotationState A) : Prop :=
    match s with
      | Idle => True
      | Done _ => True
      | Reversing ok f f' r r' => (length f' = length r') /\ (S (length f) = length r)
      | Appending ok f' r' => True
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
        inv_state state /\
        (idling state -> lenf = length f) /\
        (not (idling state) -> leb (S lenr) lenf = true)          
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


  Fixpoint take {A : Type} n (xs : list A) :=
    match n with
      | O => nil
      | S n' =>
        match xs with
          | nil => nil
          | x::xs' =>  x :: (take n' xs')
        end
    end.
               

  Fixpoint drop {A : Type} n (xs : list A) :=
    match n with
      | O => xs
      | S n =>
        match xs with
          | nil => nil
          | _ :: xs' => drop n xs'
        end
    end.

  Lemma unfold_drop_base : 
    forall A xs, 
      @drop A O xs = xs.
  Proof. reflexivity. Qed.

  Lemma unfold_drop_step :
    forall A n xs,
      @drop A (S n) xs = match xs with
                           | nil => nil
                           | x :: xs' => @drop A n xs'
                         end.
  Proof. reflexivity. Qed.

  Lemma drop_nil :
    forall A n,
      @drop A n nil = nil.
  Proof. destruct n; reflexivity. Qed.

  Lemma drop_S_cons :
    forall A n x xs,
      @drop A (S n) (x :: xs) = @drop A n xs. 
  Proof. reflexivity. Qed.

  Definition elements_in {A : Type} (s : @RotationState A) :=
    match s with
      |  Appending 0 _ r' =>
         r'
      | Appending ok f' r' =>
        drop ok (rev f' ++ r')
      | Reversing ok f f' r r' =>
        drop (S ok) (rev f' ++ f ++ rev r ++ r')

      | _ => nil
    end.

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

  Lemma exec_changes_nothing :
    forall A lenf (f : list A) lenr r state l,
      represents (lenf, f, lenr, r, state) l ->
      represents (lenf, f, lenr, r, (exec state)) l.
    Proof.
      intros A lenf f lenr r state l Hrep.
      unfold represents in Hrep.
      unfold to_list in Hrep.
      destruct state as [ | valid front front' rear rear'| valid front' rear' | front].

        unfold exec.
        assumption.

        unfold exec.
        unfold represents; unfold to_list.
        rewrite <- Hrep.
        destruct front.
        destruct rear.
        reflexivity.
        destruct rear.
        destruct valid. 
        reflexivity.
        reflexivity.
        reflexivity.
        destruct rear.
        reflexivity.
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

  Lemma exec2_changes_nothing :
    forall A lenf (f : list A) lenr r state l,
      represents (lenf, f, lenr, r, state) l ->
      represents (exec2 (lenf, f, lenr, r, state)) l.
    Proof.
      unfold exec2.
      intros.
      case_eq (exec (exec state));
      intros;
      apply exec_changes_nothing in H;
      apply exec_changes_nothing in H;
      rewrite H0 in H;
      unfold represents, to_list in H;
      unfold represents, to_list;
      assumption.      
    Qed.
    
  Lemma empty_inv :
    forall A, 
      inv (@empty A). 
  Proof.
  Admitted.

          
  Lemma inject_inv :
    forall A (q : Q A) x,
      inv q ->
      inv (inject x q).
  Proof.
    unfold inv.
    destruct q as ((((lenf, f), lenr), r), state).
  Admitted.

    
  Lemma pop_inv :
    forall A (q : Q A),
      inv q ->
      inv (pop q).
  Proof. Admitted.
    
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
    apply exec2_changes_nothing.
    rewrite <- Hrep.
    unfold represents; unfold to_list.
    rewrite H.
    rewrite app_assoc.
    reflexivity.

    apply exec2_changes_nothing.
    rewrite <- Hrep.
    unfold represents; unfold to_list.
    rewrite H.
    simpl.
    rewrite ? app_nil_r.
    rewrite app_assoc.
    reflexivity.

    rewrite Hinv5.
    apply exec2_changes_nothing.
    rewrite <- Hrep.
    unfold represents; unfold to_list.
    rewrite H.
    simpl.
    rewrite app_assoc.
    reflexivity.
    auto.

    rewrite Hinv5.
    apply exec2_changes_nothing.
    rewrite <- Hrep.
    unfold represents; unfold to_list.
    rewrite H.
    rewrite app_assoc.
    reflexivity.
    auto.
    
    rewrite Hinv5.
    apply exec2_changes_nothing.
    rewrite <- Hrep.
    unfold represents; unfold to_list.
    rewrite H.
    rewrite app_assoc.
    reflexivity.
    auto.
Qed.
  
  Lemma pop_empty :
    forall A (q : Q A),
      represents q nil ->
      inv q ->
      represents (pop q) nil.
  Proof.
    intros A [[[[lenf f] lenr] r] state].
    intros Hrep Hinv.
    destruct Hinv as [Hinv1 [Hinv2 [Hinv3 [Hinv4 Hinv5]]]].

    assert (Hf : f = nil).
      unfold represents in Hrep.
      unfold to_list in Hrep.
      destruct state as [ | valid front front' rear rear'| valid front' rear' | front].
        apply app_eq_nil in Hrep.
        destruct Hrep; assumption.
        unfold inv_state in Hinv3.
        destruct Hinv3 as [Hinv3A Hinv3B].
        apply app_eq_nil in Hrep.
        destruct Hrep.
        apply app_eq_nil in H.
        destruct H.
        apply app_eq_nil in H1.
        destruct H1.
        subst.
        simpl in Hinv3B.
        apply app_eq_nil in H2.
        destruct H2.
        assert (forall A (xs : list A), rev xs = nil -> xs = 
nil).
        intros.
        destruct xs.
        reflexivity.
        simpl in H3.
        apply app_eq_nil in H3.
        destruct H3.
        inversion H4.
        rewrite (H3 _ rear) in Hinv3B.
        inversion Hinv3B.
        assumption.
        

    subst.

    unfold idling in Hinv5.
    unfold not in Hinv5.
    assert (False -> False).
    intro F.
    apply F.
    specialize (Hinv5 H).
    unfold inv_state in Hinv3.


    unfold pop.

    unfold represents in Hrep.
    unfold to_list in Hrep.
    apply app_eq_nil in Hrep.

    rewrite Hf.
    subst.
    assumption.
 destruct f.
    unfold represents.
    unfold to_list.
    unfold represents in Hrep; unfold to_list in Hrep.

    assumption.
    assumption.
    assumption.
    assumption.
    unfold check.
    remember (leb lenr (lenf - 1)) as b.
    destruct b.
    apply exec2_changes_nothing.

    unfold represents.
    unfold to_list.
    unfold represents in Hrep; unfold to_list in Hrep.
    unfold check.
    remember
    destruct state as [ | valid front front' rear rear'| valid front' rear' | front].



    remember (leb (S _) _).
    destruct b.
    apply exec2_changes_nothing.
    rewrite <- Hrep.
    unfold represents; unfold to_list.
    rewrite H.
    rewrite app_assoc.
    reflexivity.



  Lemma pop_xs :
    forall A (q : Q A) x xs,
      represents q (x::xs) ->
      inv q ->
      represents (pop q) xs.
  Proof. Admitted.      
  
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