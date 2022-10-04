module XbarLabels where

import XbarUtils
import Data.Text (Text)
import Data.Text qualified as T

_V :: Label
_V = Label (Head HV) []

_V' :: Label
_V' = Label (X' $ Head HV) []

_VP :: Label
_VP = Label (XP $ Head HV) []

_Q :: Label
_Q = Label (Head HQ) []

_QP :: Label
_QP = Label (XP $ Head HQ) []

_v :: Label
_v = Label (Head Hv) []

_v' :: Label
_v' = Label (X' $ Head Hv) []

_vP :: Label
_vP = Label (XP $ Head Hv) []

_D :: Label
_D = Label (Head HD) []

_DP :: Label
_DP = Label (XP $ Head HD) []

_F :: Label
_F = Label (Head HF) []

_FP :: Label
_FP = Label (XP $ Head HF) []

_Fplus :: Label
_Fplus = Label (Xplus $ Head HF) []

_Co :: Label
_Co = Label (Head HCo) []

_Co' :: Label
_Co' = Label (X' $ Head HCo) []

_CoP :: Label
_CoP = Label (XP $ Head HCo) []

_CoF :: Label
_CoF = Label (X_F $ Head HCo) []

_Topic :: Label
_Topic = Label (Head HTopic) []

_Topic' :: Label
_Topic' = Label (X' $ Head HTopic) []

_TopicP :: Label
_TopicP = Label (XP $ Head HTopic) []

_Adv :: Label
_Adv = Label (Head HAdv) []

_AdvP :: Label
_AdvP = Label (XP $ Head HAdv) []

_P :: Label
_P = Label (Head HP) []

_PP :: Label
_PP = Label (XP $ Head HP) []

_Foc :: Label
_Foc = Label (Head HFoc) []

_FocP :: Label
_FocP = Label (XP $ Head HFoc) []

_SA :: Label
_SA = Label (Head HSA) []

_SAP :: Label
_SAP = Label (XP $ Head HSA) []

_Free :: Label
_Free = Label (Head HFree) []

_FreeP :: Label
_FreeP = Label (XP $ Head HFree) []

_C :: Label
_C = Label (Head HC) []

_CP :: Label
_CP = Label (XP $ Head HC) []

_CPλ :: Label
_CPλ = Label (XP $ Head HC) [PlusLambda]
