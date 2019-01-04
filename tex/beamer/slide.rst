####################################################################
Alternating Finite Automata for Deciding Presburger Arithmetic
####################################################################

:Authors: - Kei Shirakizawa (JAIST)

.. role:: raw(raw)
   :format: latex

.. default-role:: math

Introduction
=============================

Automata-based Decision Procedure for Presburger Arithmetic
--------------------------------------------------------------------
:raw:`\todo{State problem specification and introduce automata-based 
decision procedure.}`

Problem
  On input a Presburger Arithmetic formula, check SAT/validity.

Procedure
  (Boudet, Comon)

  #. Regular set as solution space
  #. Check language emptiness

Optimized Methods for Automata-based Decision Procedure
--------------------------------------------------------------------
:raw:`\todo{Introduce Antichain and Bisimulation}`

.. admonition:: Background

   TODO

.. admonition:: Antichain algorithm

   TODO

.. admonition:: Bisimulation up to congruence

   TODO

Topics
-----------------------------

Problem
  SAT/validity checking of Presburger Arithmetic

Procedure
  Automata construction followed by emptiness checking

Limitation
  EXPTIME-COMPLETE

Aim
  Optimize the decision procedure

.. admonition:: Optimization techniques

   - **Antichain Algorithm**: Language universality
   - **Bisimulation up to Congruence**: Language equality

.. admonition:: Contribution

   - Alternating Finite Automaton(AFA)
   - SAT encoding

Related Works
-----------------------------
:raw:`\todo{Collect works on automatic structure, optimization, 
decision procedures}`

.. admonition:: So-called Automatic Structure

   WS1S is a logic with a model

.. admonition:: Optimization Techniques

   Nested Antichain for WS1S

.. admonition:: Decision Procedures

   Coalgebraic approach

Preliminaries
=============================

Presburger Arithmetic
-----------------------------
:raw:`\todo{Define PA}`  
`\forall x \ldotp` and `\exists y \ldotp`.

.. raw:: latex

  \begin{example}[Presburger Arithmetic]
       \todo{Show example}
  \end{example}

Finite Automata
-----------------------------
:raw:`\todo{Define FA}`  

.. raw:: latex

   \begin{definition}[NFA]
       \todo{Define NFA}
   \end{definition}

   \begin{example}[NFA]
       \todo{Show example}
   \end{example}

Automata-based Decision Procedure for Presburger Arithmetic
--------------------------------------------------------------------
:raw:`\todo{Explain/demonstrate Boudet Comon}`

.. admonition:: Boudet Common

   Automata onstruction

Projection for Existential Quantifier
--------------------------------------------------------------------
:raw:`\todo{Explain/demonstrate homomorphism and projection}`

.. raw:: latex

   \begin{theorem}[Closed under homomorphism]
     A regular language is closed under homomorphism.
   \end{theorem}

Optimization Techniques
=============================

Antichain Algorithm
-----------------------------

.. raw:: latex

   \( \subseteq \)

.. image:: images/nano.pdf

Bisimulation up to Congruence
-----------------------------

.. raw:: latex

   \( \cong \)

.. image:: images/nano.pdf

AFA-based Decision Procedure
=====================================================================

Alternating Finite Automaton
--------------------------------------------------------------------
:raw:`todo{Define AFA and explain/demonstrate NFA equivalence.}`

NFA to AFA Translation
-----------------------------
:raw:`todo{Explain/demonstrate AFA translation.}`

AC in AFA
-----------------------------
:raw:`\todo{Explain/demonstrate AC in AFA setting}`

.. raw:: latex

   \( 
       \alpha \equiv q_0[\delta(q_0, a)/q_0, \ldots \delta(q_n, a)/q_n]
   \)
   \( \alpha \Rightarrow \beta \)

BC in AFA
-----------------------------
:raw:`\todo{Explain/demonstrate BC in AFA setting}`

.. raw:: latex

   \( 
       \alpha \equiv q_0[\delta(q_0, a)/q_0, \ldots \delta(q_n, a)/q_n]
   \)
   \( \alpha \vee \gamma \Rightarrow \beta \)

AFA-based Optimization Method
--------------------------------------------------------------------
:raw:`\todo{Define the procedure}`

Example (AFA-based Optimization Method)
--------------------------------------------------------------------
:raw:`\todo{Exemplify AFA-based Optimization Method}`

SAT Encoding
-----------------------------
:raw:`\todo{Explain/demonstrate SAT encoding}`

Example (SAT Encoding)
-----------------------------
:raw:`\todo{Exemplify SAT encoding}`

Conclusion 
=============================

Evaluation
-----------------------------
:raw:`\todo{Evaluate experimental result}`

Nothing to evaluate.

Conclusion
-----------------------------
:raw:`\todo{Conclude contribution of this work}`

Problem
  Check `\varphi \models \mathit{PA}`

Procedure
  #. DFA -> AFA
  #. Enumerate AFA's reachable state minimizing it
  #. Ordering -> CNF and feed it to SAT solver

.. admonition:: Comparison of techniques

   - Antichain Algorithm: Check `\mathcal{L}(\mathcal{A}) = \Sigma^*`
   - Bisimulation up to Congruence: `\mathcal{L}(\mathcal{A}) = \mathcal{L}(\mathcal{B})`
   - **AFA-based technique**: `\mathcal{L}(\mathcal{A}) = \varnothing`

Future work
-----------------------------
:raw:`\todo{Enumerate further direction of this work}`

.. admonition:: So-called Automatic Structure

   WS1S is a logic with a model

.. admonition:: Optimization Techniques

   Nested Antichain for WS1S

.. admonition:: Decision Procedures

   Coalgebraic approach
