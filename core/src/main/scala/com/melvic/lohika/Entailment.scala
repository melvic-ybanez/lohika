package com.melvic.lohika

import com.melvic.lohika.formula.Formula

final case class Entailment(assumptions: List[Formula], proposition: Formula)