��
��
:
Add
x"T
y"T
z"T"
Ttype:
2	
W
AddN
inputs"T*N
sum"T"
Nint(0"!
Ttype:
2	��
�
ApplyAdagrad
var"T�
accum"T�
lr"T	
grad"T
out"T�" 
Ttype:
2	"
use_lockingbool( "
update_slotsbool(
�
ApplyGradientDescent
var"T�

alpha"T

delta"T
out"T�" 
Ttype:
2	"
use_lockingbool( 
�
ArgMax

input"T
	dimension"Tidx
output"output_type" 
Ttype:
2	"
Tidxtype0:
2	"
output_typetype0	:
2	
x
Assign
ref"T�

value"T

output_ref"T�"	
Ttype"
validate_shapebool("
use_lockingbool(�
R
BroadcastGradientArgs
s0"T
s1"T
r0"T
r1"T"
Ttype0:
2	
N
Cast	
x"SrcT	
y"DstT"
SrcTtype"
DstTtype"
Truncatebool( 
8
Const
output"dtype"
valuetensor"
dtypetype
B
Equal
x"T
y"T
z
"
Ttype:
2	
�
,
Exp
x"T
y"T"
Ttype:

2
^
Fill
dims"
index_type

value"T
output"T"	
Ttype"

index_typetype0:
2	
.
Identity

input"T
output"T"	
Ttype
,
Log
x"T
y"T"
Ttype:

2
p
MatMul
a"T
b"T
product"T"
transpose_abool( "
transpose_bbool( "
Ttype:
	2
�
Mean

input"T
reduction_indices"Tidx
output"T"
	keep_dimsbool( " 
Ttype:
2	"
Tidxtype0:
2	
e
MergeV2Checkpoints
checkpoint_prefixes
destination_prefix"
delete_old_dirsbool(�
=
Mul
x"T
y"T
z"T"
Ttype:
2	�
.
Neg
x"T
y"T"
Ttype:

2	

NoOp
M
Pack
values"T*N
output"T"
Nint(0"	
Ttype"
axisint 
C
Placeholder
output"dtype"
dtypetype"
shapeshape:
6
Pow
x"T
y"T
z"T"
Ttype:

2	
>
RealDiv
x"T
y"T
z"T"
Ttype:
2	
5

Reciprocal
x"T
y"T"
Ttype:

2	
[
Reshape
tensor"T
shape"Tshape
output"T"	
Ttype"
Tshapetype0:
2	
o
	RestoreV2

prefix
tensor_names
shape_and_slices
tensors2dtypes"
dtypes
list(type)(0�
0
Round
x"T
y"T"
Ttype:

2	
l
SaveV2

prefix
tensor_names
shape_and_slices
tensors2dtypes"
dtypes
list(type)(0�
P
Shape

input"T
output"out_type"	
Ttype"
out_typetype0:
2	
H
ShardedFilename
basename	
shard

num_shards
filename
0
Sigmoid
x"T
y"T"
Ttype:

2
=
SigmoidGrad
y"T
dy"T
z"T"
Ttype:

2
1
Square
x"T
y"T"
Ttype:

2	
N

StringJoin
inputs*N

output"
Nint(0"
	separatorstring 
:
Sub
x"T
y"T
z"T"
Ttype:
2	
�
Sum

input"T
reduction_indices"Tidx
output"T"
	keep_dimsbool( " 
Ttype:
2	"
Tidxtype0:
2	
c
Tile

input"T
	multiples"
Tmultiples
output"T"	
Ttype"

Tmultiplestype0:
2	
s

VariableV2
ref"dtype�"
shapeshape"
dtypetype"
	containerstring "
shared_namestring �"serve*1.12.02v1.12.0-rc2-3-ga6d8ffae09��
�
ConstConst*Q
valueHBF<"<                                                 *
dtype0*
_output_shapes
:<
b
Reshape/shapeConst*!
valueB"         *
dtype0*
_output_shapes
:
c
ReshapeReshapeConstReshape/shape*"
_output_shapes
:*
T0*
Tshape0
n
PlaceholderPlaceholder*
dtype0*'
_output_shapes
:���������*
shape:���������
p
Placeholder_1Placeholder*
dtype0*'
_output_shapes
:���������*
shape:���������
Z
zerosConst*
valueB*    *
dtype0*
_output_shapes

:
|
Variable
VariableV2*
dtype0*
	container *
_output_shapes

:*
shape
:*
shared_name 
�
Variable/AssignAssignVariablezeros*
use_locking(*
T0*
_class
loc:@Variable*
validate_shape(*
_output_shapes

:
i
Variable/readIdentityVariable*
_class
loc:@Variable*
_output_shapes

:*
T0
T
zeros_1Const*
valueB*    *
dtype0*
_output_shapes
:
v

Variable_1
VariableV2*
	container *
_output_shapes
:*
shape:*
shared_name *
dtype0
�
Variable_1/AssignAssign
Variable_1zeros_1*
use_locking(*
T0*
_class
loc:@Variable_1*
validate_shape(*
_output_shapes
:
k
Variable_1/readIdentity
Variable_1*
_class
loc:@Variable_1*
_output_shapes
:*
T0
�
MatMulMatMulPlaceholderVariable/read*
T0*
transpose_a( *'
_output_shapes
:���������*
transpose_b( 
U
addAddMatMulVariable_1/read*
T0*'
_output_shapes
:���������
I
SigmoidSigmoidadd*
T0*'
_output_shapes
:���������
E
LogLogSigmoid*
T0*'
_output_shapes
:���������
P
mulMulPlaceholder_1Log*
T0*'
_output_shapes
:���������
A
NegNegmul*'
_output_shapes
:���������*
T0
J
sub/xConst*
_output_shapes
: *
valueB
 *  �?*
dtype0
R
subSubsub/xPlaceholder_1*
T0*'
_output_shapes
:���������
L
sub_1/xConst*
_output_shapes
: *
valueB
 *  �?*
dtype0
P
sub_1Subsub_1/xSigmoid*
T0*'
_output_shapes
:���������
E
Log_1Logsub_1*
T0*'
_output_shapes
:���������
J
mul_1MulsubLog_1*
T0*'
_output_shapes
:���������
J
sub_2SubNegmul_1*
T0*'
_output_shapes
:���������
X
Const_1Const*
valueB"       *
dtype0*
_output_shapes
:
X
SumSumsub_2Const_1*
T0*
_output_shapes
: *

Tidx0*
	keep_dims( 
2
initNoOp^Variable/Assign^Variable_1/Assign
R
gradients/ShapeConst*
valueB *
dtype0*
_output_shapes
: 
X
gradients/grad_ys_0Const*
valueB
 *  �?*
dtype0*
_output_shapes
: 
o
gradients/FillFillgradients/Shapegradients/grad_ys_0*

index_type0*
_output_shapes
: *
T0
q
 gradients/Sum_grad/Reshape/shapeConst*
valueB"      *
dtype0*
_output_shapes
:
�
gradients/Sum_grad/ReshapeReshapegradients/Fill gradients/Sum_grad/Reshape/shape*
T0*
Tshape0*
_output_shapes

:
]
gradients/Sum_grad/ShapeShapesub_2*
T0*
out_type0*
_output_shapes
:
�
gradients/Sum_grad/TileTilegradients/Sum_grad/Reshapegradients/Sum_grad/Shape*

Tmultiples0*
T0*'
_output_shapes
:���������
]
gradients/sub_2_grad/ShapeShapeNeg*
T0*
out_type0*
_output_shapes
:
a
gradients/sub_2_grad/Shape_1Shapemul_1*
T0*
out_type0*
_output_shapes
:
�
*gradients/sub_2_grad/BroadcastGradientArgsBroadcastGradientArgsgradients/sub_2_grad/Shapegradients/sub_2_grad/Shape_1*
T0*2
_output_shapes 
:���������:���������
�
gradients/sub_2_grad/SumSumgradients/Sum_grad/Tile*gradients/sub_2_grad/BroadcastGradientArgs*
T0*
_output_shapes
:*

Tidx0*
	keep_dims( 
�
gradients/sub_2_grad/ReshapeReshapegradients/sub_2_grad/Sumgradients/sub_2_grad/Shape*'
_output_shapes
:���������*
T0*
Tshape0
�
gradients/sub_2_grad/Sum_1Sumgradients/Sum_grad/Tile,gradients/sub_2_grad/BroadcastGradientArgs:1*
_output_shapes
:*

Tidx0*
	keep_dims( *
T0
^
gradients/sub_2_grad/NegNeggradients/sub_2_grad/Sum_1*
T0*
_output_shapes
:
�
gradients/sub_2_grad/Reshape_1Reshapegradients/sub_2_grad/Neggradients/sub_2_grad/Shape_1*
T0*
Tshape0*'
_output_shapes
:���������
m
%gradients/sub_2_grad/tuple/group_depsNoOp^gradients/sub_2_grad/Reshape^gradients/sub_2_grad/Reshape_1
�
-gradients/sub_2_grad/tuple/control_dependencyIdentitygradients/sub_2_grad/Reshape&^gradients/sub_2_grad/tuple/group_deps*
T0*/
_class%
#!loc:@gradients/sub_2_grad/Reshape*'
_output_shapes
:���������
�
/gradients/sub_2_grad/tuple/control_dependency_1Identitygradients/sub_2_grad/Reshape_1&^gradients/sub_2_grad/tuple/group_deps*1
_class'
%#loc:@gradients/sub_2_grad/Reshape_1*'
_output_shapes
:���������*
T0
~
gradients/Neg_grad/NegNeg-gradients/sub_2_grad/tuple/control_dependency*
T0*'
_output_shapes
:���������
]
gradients/mul_1_grad/ShapeShapesub*
_output_shapes
:*
T0*
out_type0
a
gradients/mul_1_grad/Shape_1ShapeLog_1*
T0*
out_type0*
_output_shapes
:
�
*gradients/mul_1_grad/BroadcastGradientArgsBroadcastGradientArgsgradients/mul_1_grad/Shapegradients/mul_1_grad/Shape_1*
T0*2
_output_shapes 
:���������:���������
�
gradients/mul_1_grad/MulMul/gradients/sub_2_grad/tuple/control_dependency_1Log_1*
T0*'
_output_shapes
:���������
�
gradients/mul_1_grad/SumSumgradients/mul_1_grad/Mul*gradients/mul_1_grad/BroadcastGradientArgs*
_output_shapes
:*

Tidx0*
	keep_dims( *
T0
�
gradients/mul_1_grad/ReshapeReshapegradients/mul_1_grad/Sumgradients/mul_1_grad/Shape*
T0*
Tshape0*'
_output_shapes
:���������
�
gradients/mul_1_grad/Mul_1Mulsub/gradients/sub_2_grad/tuple/control_dependency_1*'
_output_shapes
:���������*
T0
�
gradients/mul_1_grad/Sum_1Sumgradients/mul_1_grad/Mul_1,gradients/mul_1_grad/BroadcastGradientArgs:1*

Tidx0*
	keep_dims( *
T0*
_output_shapes
:
�
gradients/mul_1_grad/Reshape_1Reshapegradients/mul_1_grad/Sum_1gradients/mul_1_grad/Shape_1*
T0*
Tshape0*'
_output_shapes
:���������
m
%gradients/mul_1_grad/tuple/group_depsNoOp^gradients/mul_1_grad/Reshape^gradients/mul_1_grad/Reshape_1
�
-gradients/mul_1_grad/tuple/control_dependencyIdentitygradients/mul_1_grad/Reshape&^gradients/mul_1_grad/tuple/group_deps*
T0*/
_class%
#!loc:@gradients/mul_1_grad/Reshape*'
_output_shapes
:���������
�
/gradients/mul_1_grad/tuple/control_dependency_1Identitygradients/mul_1_grad/Reshape_1&^gradients/mul_1_grad/tuple/group_deps*
T0*1
_class'
%#loc:@gradients/mul_1_grad/Reshape_1*'
_output_shapes
:���������
e
gradients/mul_grad/ShapeShapePlaceholder_1*
out_type0*
_output_shapes
:*
T0
]
gradients/mul_grad/Shape_1ShapeLog*
T0*
out_type0*
_output_shapes
:
�
(gradients/mul_grad/BroadcastGradientArgsBroadcastGradientArgsgradients/mul_grad/Shapegradients/mul_grad/Shape_1*
T0*2
_output_shapes 
:���������:���������
l
gradients/mul_grad/MulMulgradients/Neg_grad/NegLog*
T0*'
_output_shapes
:���������
�
gradients/mul_grad/SumSumgradients/mul_grad/Mul(gradients/mul_grad/BroadcastGradientArgs*
_output_shapes
:*

Tidx0*
	keep_dims( *
T0
�
gradients/mul_grad/ReshapeReshapegradients/mul_grad/Sumgradients/mul_grad/Shape*
T0*
Tshape0*'
_output_shapes
:���������
x
gradients/mul_grad/Mul_1MulPlaceholder_1gradients/Neg_grad/Neg*
T0*'
_output_shapes
:���������
�
gradients/mul_grad/Sum_1Sumgradients/mul_grad/Mul_1*gradients/mul_grad/BroadcastGradientArgs:1*
T0*
_output_shapes
:*

Tidx0*
	keep_dims( 
�
gradients/mul_grad/Reshape_1Reshapegradients/mul_grad/Sum_1gradients/mul_grad/Shape_1*
Tshape0*'
_output_shapes
:���������*
T0
g
#gradients/mul_grad/tuple/group_depsNoOp^gradients/mul_grad/Reshape^gradients/mul_grad/Reshape_1
�
+gradients/mul_grad/tuple/control_dependencyIdentitygradients/mul_grad/Reshape$^gradients/mul_grad/tuple/group_deps*
T0*-
_class#
!loc:@gradients/mul_grad/Reshape*'
_output_shapes
:���������
�
-gradients/mul_grad/tuple/control_dependency_1Identitygradients/mul_grad/Reshape_1$^gradients/mul_grad/tuple/group_deps*
T0*/
_class%
#!loc:@gradients/mul_grad/Reshape_1*'
_output_shapes
:���������
�
gradients/Log_1_grad/Reciprocal
Reciprocalsub_10^gradients/mul_1_grad/tuple/control_dependency_1*
T0*'
_output_shapes
:���������
�
gradients/Log_1_grad/mulMul/gradients/mul_1_grad/tuple/control_dependency_1gradients/Log_1_grad/Reciprocal*
T0*'
_output_shapes
:���������
�
gradients/Log_grad/Reciprocal
ReciprocalSigmoid.^gradients/mul_grad/tuple/control_dependency_1*'
_output_shapes
:���������*
T0
�
gradients/Log_grad/mulMul-gradients/mul_grad/tuple/control_dependency_1gradients/Log_grad/Reciprocal*
T0*'
_output_shapes
:���������
]
gradients/sub_1_grad/ShapeConst*
dtype0*
_output_shapes
: *
valueB 
c
gradients/sub_1_grad/Shape_1ShapeSigmoid*
out_type0*
_output_shapes
:*
T0
�
*gradients/sub_1_grad/BroadcastGradientArgsBroadcastGradientArgsgradients/sub_1_grad/Shapegradients/sub_1_grad/Shape_1*
T0*2
_output_shapes 
:���������:���������
�
gradients/sub_1_grad/SumSumgradients/Log_1_grad/mul*gradients/sub_1_grad/BroadcastGradientArgs*

Tidx0*
	keep_dims( *
T0*
_output_shapes
:
�
gradients/sub_1_grad/ReshapeReshapegradients/sub_1_grad/Sumgradients/sub_1_grad/Shape*
_output_shapes
: *
T0*
Tshape0
�
gradients/sub_1_grad/Sum_1Sumgradients/Log_1_grad/mul,gradients/sub_1_grad/BroadcastGradientArgs:1*
_output_shapes
:*

Tidx0*
	keep_dims( *
T0
^
gradients/sub_1_grad/NegNeggradients/sub_1_grad/Sum_1*
_output_shapes
:*
T0
�
gradients/sub_1_grad/Reshape_1Reshapegradients/sub_1_grad/Neggradients/sub_1_grad/Shape_1*'
_output_shapes
:���������*
T0*
Tshape0
m
%gradients/sub_1_grad/tuple/group_depsNoOp^gradients/sub_1_grad/Reshape^gradients/sub_1_grad/Reshape_1
�
-gradients/sub_1_grad/tuple/control_dependencyIdentitygradients/sub_1_grad/Reshape&^gradients/sub_1_grad/tuple/group_deps*
T0*/
_class%
#!loc:@gradients/sub_1_grad/Reshape*
_output_shapes
: 
�
/gradients/sub_1_grad/tuple/control_dependency_1Identitygradients/sub_1_grad/Reshape_1&^gradients/sub_1_grad/tuple/group_deps*'
_output_shapes
:���������*
T0*1
_class'
%#loc:@gradients/sub_1_grad/Reshape_1
�
gradients/AddNAddNgradients/Log_grad/mul/gradients/sub_1_grad/tuple/control_dependency_1*
N*'
_output_shapes
:���������*
T0*)
_class
loc:@gradients/Log_grad/mul
|
"gradients/Sigmoid_grad/SigmoidGradSigmoidGradSigmoidgradients/AddN*
T0*'
_output_shapes
:���������
^
gradients/add_grad/ShapeShapeMatMul*
out_type0*
_output_shapes
:*
T0
d
gradients/add_grad/Shape_1Const*
valueB:*
dtype0*
_output_shapes
:
�
(gradients/add_grad/BroadcastGradientArgsBroadcastGradientArgsgradients/add_grad/Shapegradients/add_grad/Shape_1*2
_output_shapes 
:���������:���������*
T0
�
gradients/add_grad/SumSum"gradients/Sigmoid_grad/SigmoidGrad(gradients/add_grad/BroadcastGradientArgs*
_output_shapes
:*

Tidx0*
	keep_dims( *
T0
�
gradients/add_grad/ReshapeReshapegradients/add_grad/Sumgradients/add_grad/Shape*
T0*
Tshape0*'
_output_shapes
:���������
�
gradients/add_grad/Sum_1Sum"gradients/Sigmoid_grad/SigmoidGrad*gradients/add_grad/BroadcastGradientArgs:1*

Tidx0*
	keep_dims( *
T0*
_output_shapes
:
�
gradients/add_grad/Reshape_1Reshapegradients/add_grad/Sum_1gradients/add_grad/Shape_1*
T0*
Tshape0*
_output_shapes
:
g
#gradients/add_grad/tuple/group_depsNoOp^gradients/add_grad/Reshape^gradients/add_grad/Reshape_1
�
+gradients/add_grad/tuple/control_dependencyIdentitygradients/add_grad/Reshape$^gradients/add_grad/tuple/group_deps*-
_class#
!loc:@gradients/add_grad/Reshape*'
_output_shapes
:���������*
T0
�
-gradients/add_grad/tuple/control_dependency_1Identitygradients/add_grad/Reshape_1$^gradients/add_grad/tuple/group_deps*/
_class%
#!loc:@gradients/add_grad/Reshape_1*
_output_shapes
:*
T0
�
gradients/MatMul_grad/MatMulMatMul+gradients/add_grad/tuple/control_dependencyVariable/read*
transpose_a( *'
_output_shapes
:���������*
transpose_b(*
T0
�
gradients/MatMul_grad/MatMul_1MatMulPlaceholder+gradients/add_grad/tuple/control_dependency*
transpose_a(*
_output_shapes

:*
transpose_b( *
T0
n
&gradients/MatMul_grad/tuple/group_depsNoOp^gradients/MatMul_grad/MatMul^gradients/MatMul_grad/MatMul_1
�
.gradients/MatMul_grad/tuple/control_dependencyIdentitygradients/MatMul_grad/MatMul'^gradients/MatMul_grad/tuple/group_deps*'
_output_shapes
:���������*
T0*/
_class%
#!loc:@gradients/MatMul_grad/MatMul
�
0gradients/MatMul_grad/tuple/control_dependency_1Identitygradients/MatMul_grad/MatMul_1'^gradients/MatMul_grad/tuple/group_deps*
_output_shapes

:*
T0*1
_class'
%#loc:@gradients/MatMul_grad/MatMul_1
b
GradientDescent/learning_rateConst*
valueB
 *���=*
dtype0*
_output_shapes
: 
�
4GradientDescent/update_Variable/ApplyGradientDescentApplyGradientDescentVariableGradientDescent/learning_rate0gradients/MatMul_grad/tuple/control_dependency_1*
use_locking( *
T0*
_class
loc:@Variable*
_output_shapes

:
�
6GradientDescent/update_Variable_1/ApplyGradientDescentApplyGradientDescent
Variable_1GradientDescent/learning_rate-gradients/add_grad/tuple/control_dependency_1*
_output_shapes
:*
use_locking( *
T0*
_class
loc:@Variable_1
�
GradientDescentNoOp5^GradientDescent/update_Variable/ApplyGradientDescent7^GradientDescent/update_Variable_1/ApplyGradientDescent
I
RoundRoundSigmoid*
T0*'
_output_shapes
:���������
V
EqualEqualRoundPlaceholder_1*
T0*'
_output_shapes
:���������
d
CastCastEqual*

SrcT0
*
Truncate( *

DstT0*'
_output_shapes
:���������
X
Const_2Const*
valueB"       *
dtype0*
_output_shapes
:
Y
MeanMeanCastConst_2*
_output_shapes
: *

Tidx0*
	keep_dims( *
T0
p
Const_3Const*1
value(B&"  �?  �?   @  @@  �@   A*
dtype0*
_output_shapes

:
T
Const_4Const*
valueB*  ��*
dtype0*
_output_shapes
:
M
sub_3SubVariable/readConst_3*
_output_shapes

:*
T0
@
SquareSquaresub_3*
_output_shapes

:*
T0
K
sub_4SubVariable_1/readConst_4*
T0*
_output_shapes
:
>
Square_1Squaresub_4*
T0*
_output_shapes
:
Y
onesConst*
valueB*  �?*
dtype0*
_output_shapes

:
~

Variable_2
VariableV2*
shared_name *
dtype0*
	container *
_output_shapes

:*
shape
:
�
Variable_2/AssignAssign
Variable_2ones*
validate_shape(*
_output_shapes

:*
use_locking(*
T0*
_class
loc:@Variable_2
o
Variable_2/readIdentity
Variable_2*
_output_shapes

:*
T0*
_class
loc:@Variable_2
[
ones_1Const*
valueB*  �?*
dtype0*
_output_shapes

:
~

Variable_3
VariableV2*
shared_name *
dtype0*
	container *
_output_shapes

:*
shape
:
�
Variable_3/AssignAssign
Variable_3ones_1*
_class
loc:@Variable_3*
validate_shape(*
_output_shapes

:*
use_locking(*
T0
o
Variable_3/readIdentity
Variable_3*
T0*
_class
loc:@Variable_3*
_output_shapes

:
p
Placeholder_2Placeholder*
shape:���������*
dtype0*'
_output_shapes
:���������
p
Placeholder_3Placeholder*
dtype0*'
_output_shapes
:���������*
shape:���������
P

Normal/locIdentityVariable_2/read*
T0*
_output_shapes

:
R
Normal/scaleIdentityVariable_3/read*
_output_shapes

:*
T0
o
Normal/prob/standardize/subSubPlaceholder_2
Normal/loc*
T0*'
_output_shapes
:���������
�
Normal/prob/standardize/truedivRealDivNormal/prob/standardize/subNormal/scale*'
_output_shapes
:���������*
T0
o
Normal/prob/SquareSquareNormal/prob/standardize/truediv*'
_output_shapes
:���������*
T0
V
Normal/prob/mul/xConst*
valueB
 *   �*
dtype0*
_output_shapes
: 
o
Normal/prob/mulMulNormal/prob/mul/xNormal/prob/Square*
T0*'
_output_shapes
:���������
M
Normal/prob/LogLogNormal/scale*
T0*
_output_shapes

:
V
Normal/prob/add/xConst*
valueB
 *�?k?*
dtype0*
_output_shapes
: 
c
Normal/prob/addAddNormal/prob/add/xNormal/prob/Log*
_output_shapes

:*
T0
j
Normal/prob/subSubNormal/prob/mulNormal/prob/add*'
_output_shapes
:���������*
T0
Y
Normal/prob/ExpExpNormal/prob/sub*
T0*'
_output_shapes
:���������
O
Log_2LogNormal/prob/Exp*'
_output_shapes
:���������*
T0
T
mul_2MulPlaceholder_3Log_2*'
_output_shapes
:���������*
T0
X
Const_5Const*
valueB"       *
dtype0*
_output_shapes
:
Z
Sum_1Summul_2Const_5*
_output_shapes
: *

Tidx0*
	keep_dims( *
T0
4
Neg_1NegSum_1*
T0*
_output_shapes
: 
J
Const_6Const*
valueB *
dtype0*
_output_shapes
: 
\
Mean_1MeanNeg_1Const_6*
T0*
_output_shapes
: *

Tidx0*
	keep_dims( 
T
gradients_1/ShapeConst*
dtype0*
_output_shapes
: *
valueB 
Z
gradients_1/grad_ys_0Const*
valueB
 *  �?*
dtype0*
_output_shapes
: 
u
gradients_1/FillFillgradients_1/Shapegradients_1/grad_ys_0*
_output_shapes
: *
T0*

index_type0
h
%gradients_1/Mean_1_grad/Reshape/shapeConst*
dtype0*
_output_shapes
: *
valueB 
�
gradients_1/Mean_1_grad/ReshapeReshapegradients_1/Fill%gradients_1/Mean_1_grad/Reshape/shape*
Tshape0*
_output_shapes
: *
T0
`
gradients_1/Mean_1_grad/ConstConst*
valueB *
dtype0*
_output_shapes
: 
�
gradients_1/Mean_1_grad/TileTilegradients_1/Mean_1_grad/Reshapegradients_1/Mean_1_grad/Const*

Tmultiples0*
T0*
_output_shapes
: 
d
gradients_1/Mean_1_grad/Const_1Const*
valueB
 *  �?*
dtype0*
_output_shapes
: 
�
gradients_1/Mean_1_grad/truedivRealDivgradients_1/Mean_1_grad/Tilegradients_1/Mean_1_grad/Const_1*
T0*
_output_shapes
: 
c
gradients_1/Neg_1_grad/NegNeggradients_1/Mean_1_grad/truediv*
T0*
_output_shapes
: 
u
$gradients_1/Sum_1_grad/Reshape/shapeConst*
valueB"      *
dtype0*
_output_shapes
:
�
gradients_1/Sum_1_grad/ReshapeReshapegradients_1/Neg_1_grad/Neg$gradients_1/Sum_1_grad/Reshape/shape*
Tshape0*
_output_shapes

:*
T0
a
gradients_1/Sum_1_grad/ShapeShapemul_2*
T0*
out_type0*
_output_shapes
:
�
gradients_1/Sum_1_grad/TileTilegradients_1/Sum_1_grad/Reshapegradients_1/Sum_1_grad/Shape*

Tmultiples0*
T0*'
_output_shapes
:���������
i
gradients_1/mul_2_grad/ShapeShapePlaceholder_3*
T0*
out_type0*
_output_shapes
:
c
gradients_1/mul_2_grad/Shape_1ShapeLog_2*
_output_shapes
:*
T0*
out_type0
�
,gradients_1/mul_2_grad/BroadcastGradientArgsBroadcastGradientArgsgradients_1/mul_2_grad/Shapegradients_1/mul_2_grad/Shape_1*
T0*2
_output_shapes 
:���������:���������
w
gradients_1/mul_2_grad/MulMulgradients_1/Sum_1_grad/TileLog_2*
T0*'
_output_shapes
:���������
�
gradients_1/mul_2_grad/SumSumgradients_1/mul_2_grad/Mul,gradients_1/mul_2_grad/BroadcastGradientArgs*

Tidx0*
	keep_dims( *
T0*
_output_shapes
:
�
gradients_1/mul_2_grad/ReshapeReshapegradients_1/mul_2_grad/Sumgradients_1/mul_2_grad/Shape*
Tshape0*'
_output_shapes
:���������*
T0
�
gradients_1/mul_2_grad/Mul_1MulPlaceholder_3gradients_1/Sum_1_grad/Tile*
T0*'
_output_shapes
:���������
�
gradients_1/mul_2_grad/Sum_1Sumgradients_1/mul_2_grad/Mul_1.gradients_1/mul_2_grad/BroadcastGradientArgs:1*

Tidx0*
	keep_dims( *
T0*
_output_shapes
:
�
 gradients_1/mul_2_grad/Reshape_1Reshapegradients_1/mul_2_grad/Sum_1gradients_1/mul_2_grad/Shape_1*
T0*
Tshape0*'
_output_shapes
:���������
s
'gradients_1/mul_2_grad/tuple/group_depsNoOp^gradients_1/mul_2_grad/Reshape!^gradients_1/mul_2_grad/Reshape_1
�
/gradients_1/mul_2_grad/tuple/control_dependencyIdentitygradients_1/mul_2_grad/Reshape(^gradients_1/mul_2_grad/tuple/group_deps*'
_output_shapes
:���������*
T0*1
_class'
%#loc:@gradients_1/mul_2_grad/Reshape
�
1gradients_1/mul_2_grad/tuple/control_dependency_1Identity gradients_1/mul_2_grad/Reshape_1(^gradients_1/mul_2_grad/tuple/group_deps*
T0*3
_class)
'%loc:@gradients_1/mul_2_grad/Reshape_1*'
_output_shapes
:���������
�
!gradients_1/Log_2_grad/Reciprocal
ReciprocalNormal/prob/Exp2^gradients_1/mul_2_grad/tuple/control_dependency_1*
T0*'
_output_shapes
:���������
�
gradients_1/Log_2_grad/mulMul1gradients_1/mul_2_grad/tuple/control_dependency_1!gradients_1/Log_2_grad/Reciprocal*
T0*'
_output_shapes
:���������
�
$gradients_1/Normal/prob/Exp_grad/mulMulgradients_1/Log_2_grad/mulNormal/prob/Exp*
T0*'
_output_shapes
:���������
u
&gradients_1/Normal/prob/sub_grad/ShapeShapeNormal/prob/mul*
out_type0*
_output_shapes
:*
T0
y
(gradients_1/Normal/prob/sub_grad/Shape_1Const*
valueB"      *
dtype0*
_output_shapes
:
�
6gradients_1/Normal/prob/sub_grad/BroadcastGradientArgsBroadcastGradientArgs&gradients_1/Normal/prob/sub_grad/Shape(gradients_1/Normal/prob/sub_grad/Shape_1*2
_output_shapes 
:���������:���������*
T0
�
$gradients_1/Normal/prob/sub_grad/SumSum$gradients_1/Normal/prob/Exp_grad/mul6gradients_1/Normal/prob/sub_grad/BroadcastGradientArgs*
_output_shapes
:*

Tidx0*
	keep_dims( *
T0
�
(gradients_1/Normal/prob/sub_grad/ReshapeReshape$gradients_1/Normal/prob/sub_grad/Sum&gradients_1/Normal/prob/sub_grad/Shape*
T0*
Tshape0*'
_output_shapes
:���������
�
&gradients_1/Normal/prob/sub_grad/Sum_1Sum$gradients_1/Normal/prob/Exp_grad/mul8gradients_1/Normal/prob/sub_grad/BroadcastGradientArgs:1*

Tidx0*
	keep_dims( *
T0*
_output_shapes
:
v
$gradients_1/Normal/prob/sub_grad/NegNeg&gradients_1/Normal/prob/sub_grad/Sum_1*
T0*
_output_shapes
:
�
*gradients_1/Normal/prob/sub_grad/Reshape_1Reshape$gradients_1/Normal/prob/sub_grad/Neg(gradients_1/Normal/prob/sub_grad/Shape_1*
T0*
Tshape0*
_output_shapes

:
�
1gradients_1/Normal/prob/sub_grad/tuple/group_depsNoOp)^gradients_1/Normal/prob/sub_grad/Reshape+^gradients_1/Normal/prob/sub_grad/Reshape_1
�
9gradients_1/Normal/prob/sub_grad/tuple/control_dependencyIdentity(gradients_1/Normal/prob/sub_grad/Reshape2^gradients_1/Normal/prob/sub_grad/tuple/group_deps*
T0*;
_class1
/-loc:@gradients_1/Normal/prob/sub_grad/Reshape*'
_output_shapes
:���������
�
;gradients_1/Normal/prob/sub_grad/tuple/control_dependency_1Identity*gradients_1/Normal/prob/sub_grad/Reshape_12^gradients_1/Normal/prob/sub_grad/tuple/group_deps*
T0*=
_class3
1/loc:@gradients_1/Normal/prob/sub_grad/Reshape_1*
_output_shapes

:
i
&gradients_1/Normal/prob/mul_grad/ShapeConst*
valueB *
dtype0*
_output_shapes
: 
z
(gradients_1/Normal/prob/mul_grad/Shape_1ShapeNormal/prob/Square*
T0*
out_type0*
_output_shapes
:
�
6gradients_1/Normal/prob/mul_grad/BroadcastGradientArgsBroadcastGradientArgs&gradients_1/Normal/prob/mul_grad/Shape(gradients_1/Normal/prob/mul_grad/Shape_1*2
_output_shapes 
:���������:���������*
T0
�
$gradients_1/Normal/prob/mul_grad/MulMul9gradients_1/Normal/prob/sub_grad/tuple/control_dependencyNormal/prob/Square*
T0*'
_output_shapes
:���������
�
$gradients_1/Normal/prob/mul_grad/SumSum$gradients_1/Normal/prob/mul_grad/Mul6gradients_1/Normal/prob/mul_grad/BroadcastGradientArgs*
T0*
_output_shapes
:*

Tidx0*
	keep_dims( 
�
(gradients_1/Normal/prob/mul_grad/ReshapeReshape$gradients_1/Normal/prob/mul_grad/Sum&gradients_1/Normal/prob/mul_grad/Shape*
T0*
Tshape0*
_output_shapes
: 
�
&gradients_1/Normal/prob/mul_grad/Mul_1MulNormal/prob/mul/x9gradients_1/Normal/prob/sub_grad/tuple/control_dependency*
T0*'
_output_shapes
:���������
�
&gradients_1/Normal/prob/mul_grad/Sum_1Sum&gradients_1/Normal/prob/mul_grad/Mul_18gradients_1/Normal/prob/mul_grad/BroadcastGradientArgs:1*
_output_shapes
:*

Tidx0*
	keep_dims( *
T0
�
*gradients_1/Normal/prob/mul_grad/Reshape_1Reshape&gradients_1/Normal/prob/mul_grad/Sum_1(gradients_1/Normal/prob/mul_grad/Shape_1*
T0*
Tshape0*'
_output_shapes
:���������
�
1gradients_1/Normal/prob/mul_grad/tuple/group_depsNoOp)^gradients_1/Normal/prob/mul_grad/Reshape+^gradients_1/Normal/prob/mul_grad/Reshape_1
�
9gradients_1/Normal/prob/mul_grad/tuple/control_dependencyIdentity(gradients_1/Normal/prob/mul_grad/Reshape2^gradients_1/Normal/prob/mul_grad/tuple/group_deps*
_output_shapes
: *
T0*;
_class1
/-loc:@gradients_1/Normal/prob/mul_grad/Reshape
�
;gradients_1/Normal/prob/mul_grad/tuple/control_dependency_1Identity*gradients_1/Normal/prob/mul_grad/Reshape_12^gradients_1/Normal/prob/mul_grad/tuple/group_deps*
T0*=
_class3
1/loc:@gradients_1/Normal/prob/mul_grad/Reshape_1*'
_output_shapes
:���������
i
&gradients_1/Normal/prob/add_grad/ShapeConst*
valueB *
dtype0*
_output_shapes
: 
y
(gradients_1/Normal/prob/add_grad/Shape_1Const*
valueB"      *
dtype0*
_output_shapes
:
�
6gradients_1/Normal/prob/add_grad/BroadcastGradientArgsBroadcastGradientArgs&gradients_1/Normal/prob/add_grad/Shape(gradients_1/Normal/prob/add_grad/Shape_1*
T0*2
_output_shapes 
:���������:���������
�
$gradients_1/Normal/prob/add_grad/SumSum;gradients_1/Normal/prob/sub_grad/tuple/control_dependency_16gradients_1/Normal/prob/add_grad/BroadcastGradientArgs*
T0*
_output_shapes
: *

Tidx0*
	keep_dims( 
�
(gradients_1/Normal/prob/add_grad/ReshapeReshape$gradients_1/Normal/prob/add_grad/Sum&gradients_1/Normal/prob/add_grad/Shape*
_output_shapes
: *
T0*
Tshape0
�
&gradients_1/Normal/prob/add_grad/Sum_1Sum;gradients_1/Normal/prob/sub_grad/tuple/control_dependency_18gradients_1/Normal/prob/add_grad/BroadcastGradientArgs:1*
T0*
_output_shapes
:*

Tidx0*
	keep_dims( 
�
*gradients_1/Normal/prob/add_grad/Reshape_1Reshape&gradients_1/Normal/prob/add_grad/Sum_1(gradients_1/Normal/prob/add_grad/Shape_1*
T0*
Tshape0*
_output_shapes

:
�
1gradients_1/Normal/prob/add_grad/tuple/group_depsNoOp)^gradients_1/Normal/prob/add_grad/Reshape+^gradients_1/Normal/prob/add_grad/Reshape_1
�
9gradients_1/Normal/prob/add_grad/tuple/control_dependencyIdentity(gradients_1/Normal/prob/add_grad/Reshape2^gradients_1/Normal/prob/add_grad/tuple/group_deps*
_output_shapes
: *
T0*;
_class1
/-loc:@gradients_1/Normal/prob/add_grad/Reshape
�
;gradients_1/Normal/prob/add_grad/tuple/control_dependency_1Identity*gradients_1/Normal/prob/add_grad/Reshape_12^gradients_1/Normal/prob/add_grad/tuple/group_deps*
T0*=
_class3
1/loc:@gradients_1/Normal/prob/add_grad/Reshape_1*
_output_shapes

:
�
)gradients_1/Normal/prob/Square_grad/ConstConst<^gradients_1/Normal/prob/mul_grad/tuple/control_dependency_1*
valueB
 *   @*
dtype0*
_output_shapes
: 
�
'gradients_1/Normal/prob/Square_grad/MulMulNormal/prob/standardize/truediv)gradients_1/Normal/prob/Square_grad/Const*
T0*'
_output_shapes
:���������
�
)gradients_1/Normal/prob/Square_grad/Mul_1Mul;gradients_1/Normal/prob/mul_grad/tuple/control_dependency_1'gradients_1/Normal/prob/Square_grad/Mul*
T0*'
_output_shapes
:���������
�
+gradients_1/Normal/prob/Log_grad/Reciprocal
ReciprocalNormal/scale<^gradients_1/Normal/prob/add_grad/tuple/control_dependency_1*
_output_shapes

:*
T0
�
$gradients_1/Normal/prob/Log_grad/mulMul;gradients_1/Normal/prob/add_grad/tuple/control_dependency_1+gradients_1/Normal/prob/Log_grad/Reciprocal*
T0*
_output_shapes

:
�
6gradients_1/Normal/prob/standardize/truediv_grad/ShapeShapeNormal/prob/standardize/sub*
_output_shapes
:*
T0*
out_type0
�
8gradients_1/Normal/prob/standardize/truediv_grad/Shape_1Const*
_output_shapes
:*
valueB"      *
dtype0
�
Fgradients_1/Normal/prob/standardize/truediv_grad/BroadcastGradientArgsBroadcastGradientArgs6gradients_1/Normal/prob/standardize/truediv_grad/Shape8gradients_1/Normal/prob/standardize/truediv_grad/Shape_1*
T0*2
_output_shapes 
:���������:���������
�
8gradients_1/Normal/prob/standardize/truediv_grad/RealDivRealDiv)gradients_1/Normal/prob/Square_grad/Mul_1Normal/scale*'
_output_shapes
:���������*
T0
�
4gradients_1/Normal/prob/standardize/truediv_grad/SumSum8gradients_1/Normal/prob/standardize/truediv_grad/RealDivFgradients_1/Normal/prob/standardize/truediv_grad/BroadcastGradientArgs*

Tidx0*
	keep_dims( *
T0*
_output_shapes
:
�
8gradients_1/Normal/prob/standardize/truediv_grad/ReshapeReshape4gradients_1/Normal/prob/standardize/truediv_grad/Sum6gradients_1/Normal/prob/standardize/truediv_grad/Shape*
T0*
Tshape0*'
_output_shapes
:���������
�
4gradients_1/Normal/prob/standardize/truediv_grad/NegNegNormal/prob/standardize/sub*
T0*'
_output_shapes
:���������
�
:gradients_1/Normal/prob/standardize/truediv_grad/RealDiv_1RealDiv4gradients_1/Normal/prob/standardize/truediv_grad/NegNormal/scale*'
_output_shapes
:���������*
T0
�
:gradients_1/Normal/prob/standardize/truediv_grad/RealDiv_2RealDiv:gradients_1/Normal/prob/standardize/truediv_grad/RealDiv_1Normal/scale*'
_output_shapes
:���������*
T0
�
4gradients_1/Normal/prob/standardize/truediv_grad/mulMul)gradients_1/Normal/prob/Square_grad/Mul_1:gradients_1/Normal/prob/standardize/truediv_grad/RealDiv_2*
T0*'
_output_shapes
:���������
�
6gradients_1/Normal/prob/standardize/truediv_grad/Sum_1Sum4gradients_1/Normal/prob/standardize/truediv_grad/mulHgradients_1/Normal/prob/standardize/truediv_grad/BroadcastGradientArgs:1*
T0*
_output_shapes
:*

Tidx0*
	keep_dims( 
�
:gradients_1/Normal/prob/standardize/truediv_grad/Reshape_1Reshape6gradients_1/Normal/prob/standardize/truediv_grad/Sum_18gradients_1/Normal/prob/standardize/truediv_grad/Shape_1*
Tshape0*
_output_shapes

:*
T0
�
Agradients_1/Normal/prob/standardize/truediv_grad/tuple/group_depsNoOp9^gradients_1/Normal/prob/standardize/truediv_grad/Reshape;^gradients_1/Normal/prob/standardize/truediv_grad/Reshape_1
�
Igradients_1/Normal/prob/standardize/truediv_grad/tuple/control_dependencyIdentity8gradients_1/Normal/prob/standardize/truediv_grad/ReshapeB^gradients_1/Normal/prob/standardize/truediv_grad/tuple/group_deps*
T0*K
_classA
?=loc:@gradients_1/Normal/prob/standardize/truediv_grad/Reshape*'
_output_shapes
:���������
�
Kgradients_1/Normal/prob/standardize/truediv_grad/tuple/control_dependency_1Identity:gradients_1/Normal/prob/standardize/truediv_grad/Reshape_1B^gradients_1/Normal/prob/standardize/truediv_grad/tuple/group_deps*
T0*M
_classC
A?loc:@gradients_1/Normal/prob/standardize/truediv_grad/Reshape_1*
_output_shapes

:

2gradients_1/Normal/prob/standardize/sub_grad/ShapeShapePlaceholder_2*
T0*
out_type0*
_output_shapes
:
�
4gradients_1/Normal/prob/standardize/sub_grad/Shape_1Const*
valueB"      *
dtype0*
_output_shapes
:
�
Bgradients_1/Normal/prob/standardize/sub_grad/BroadcastGradientArgsBroadcastGradientArgs2gradients_1/Normal/prob/standardize/sub_grad/Shape4gradients_1/Normal/prob/standardize/sub_grad/Shape_1*2
_output_shapes 
:���������:���������*
T0
�
0gradients_1/Normal/prob/standardize/sub_grad/SumSumIgradients_1/Normal/prob/standardize/truediv_grad/tuple/control_dependencyBgradients_1/Normal/prob/standardize/sub_grad/BroadcastGradientArgs*
_output_shapes
:*

Tidx0*
	keep_dims( *
T0
�
4gradients_1/Normal/prob/standardize/sub_grad/ReshapeReshape0gradients_1/Normal/prob/standardize/sub_grad/Sum2gradients_1/Normal/prob/standardize/sub_grad/Shape*
T0*
Tshape0*'
_output_shapes
:���������
�
2gradients_1/Normal/prob/standardize/sub_grad/Sum_1SumIgradients_1/Normal/prob/standardize/truediv_grad/tuple/control_dependencyDgradients_1/Normal/prob/standardize/sub_grad/BroadcastGradientArgs:1*

Tidx0*
	keep_dims( *
T0*
_output_shapes
:
�
0gradients_1/Normal/prob/standardize/sub_grad/NegNeg2gradients_1/Normal/prob/standardize/sub_grad/Sum_1*
_output_shapes
:*
T0
�
6gradients_1/Normal/prob/standardize/sub_grad/Reshape_1Reshape0gradients_1/Normal/prob/standardize/sub_grad/Neg4gradients_1/Normal/prob/standardize/sub_grad/Shape_1*
T0*
Tshape0*
_output_shapes

:
�
=gradients_1/Normal/prob/standardize/sub_grad/tuple/group_depsNoOp5^gradients_1/Normal/prob/standardize/sub_grad/Reshape7^gradients_1/Normal/prob/standardize/sub_grad/Reshape_1
�
Egradients_1/Normal/prob/standardize/sub_grad/tuple/control_dependencyIdentity4gradients_1/Normal/prob/standardize/sub_grad/Reshape>^gradients_1/Normal/prob/standardize/sub_grad/tuple/group_deps*
T0*G
_class=
;9loc:@gradients_1/Normal/prob/standardize/sub_grad/Reshape*'
_output_shapes
:���������
�
Ggradients_1/Normal/prob/standardize/sub_grad/tuple/control_dependency_1Identity6gradients_1/Normal/prob/standardize/sub_grad/Reshape_1>^gradients_1/Normal/prob/standardize/sub_grad/tuple/group_deps*
_output_shapes

:*
T0*I
_class?
=;loc:@gradients_1/Normal/prob/standardize/sub_grad/Reshape_1
�
gradients_1/AddNAddN$gradients_1/Normal/prob/Log_grad/mulKgradients_1/Normal/prob/standardize/truediv_grad/tuple/control_dependency_1*
_output_shapes

:*
T0*7
_class-
+)loc:@gradients_1/Normal/prob/Log_grad/mul*
N
�
$Variable_2/Adagrad/Initializer/ConstConst*
valueB*���=*
_class
loc:@Variable_2*
dtype0*
_output_shapes

:
�
Variable_2/Adagrad
VariableV2*
_class
loc:@Variable_2*
	container *
shape
:*
dtype0*
_output_shapes

:*
shared_name 
�
Variable_2/Adagrad/AssignAssignVariable_2/Adagrad$Variable_2/Adagrad/Initializer/Const*
T0*
_class
loc:@Variable_2*
validate_shape(*
_output_shapes

:*
use_locking(

Variable_2/Adagrad/readIdentityVariable_2/Adagrad*
_class
loc:@Variable_2*
_output_shapes

:*
T0
�
$Variable_3/Adagrad/Initializer/ConstConst*
valueB*���=*
_class
loc:@Variable_3*
dtype0*
_output_shapes

:
�
Variable_3/Adagrad
VariableV2*
dtype0*
_output_shapes

:*
shared_name *
_class
loc:@Variable_3*
	container *
shape
:
�
Variable_3/Adagrad/AssignAssignVariable_3/Adagrad$Variable_3/Adagrad/Initializer/Const*
use_locking(*
T0*
_class
loc:@Variable_3*
validate_shape(*
_output_shapes

:

Variable_3/Adagrad/readIdentityVariable_3/Adagrad*
T0*
_class
loc:@Variable_3*
_output_shapes

:
Z
Adagrad/learning_rateConst*
dtype0*
_output_shapes
: *
valueB
 *ff�>
�
&Adagrad/update_Variable_2/ApplyAdagradApplyAdagrad
Variable_2Variable_2/AdagradAdagrad/learning_rateGgradients_1/Normal/prob/standardize/sub_grad/tuple/control_dependency_1*
update_slots(*
_output_shapes

:*
use_locking( *
T0*
_class
loc:@Variable_2
�
&Adagrad/update_Variable_3/ApplyAdagradApplyAdagrad
Variable_3Variable_3/AdagradAdagrad/learning_rategradients_1/AddN*
use_locking( *
T0*
_class
loc:@Variable_3*
update_slots(*
_output_shapes

:
a
AdagradNoOp'^Adagrad/update_Variable_2/ApplyAdagrad'^Adagrad/update_Variable_3/ApplyAdagrad
�
init_1NoOp^Variable/Assign^Variable_1/Assign^Variable_2/Adagrad/Assign^Variable_2/Assign^Variable_3/Adagrad/Assign^Variable_3/Assign
d
Const_7Const*
dtype0*
_output_shapes

:*%
valueB"  ��      @@
d
Const_8Const*%
valueB"   ?  �?  �?*
dtype0*
_output_shapes

:
O
sub_5SubVariable_2/readConst_7*
T0*
_output_shapes

:
J
pow/yConst*
valueB
 *   @*
dtype0*
_output_shapes
: 
A
powPowsub_5pow/y*
_output_shapes

:*
T0
X
Const_9Const*
valueB"       *
dtype0*
_output_shapes
:
X
Sum_2SumpowConst_9*
_output_shapes
: *

Tidx0*
	keep_dims( *
T0
O
sub_6SubVariable_3/readConst_8*
T0*
_output_shapes

:
L
pow_1/yConst*
valueB
 *   @*
dtype0*
_output_shapes
: 
E
pow_1Powsub_6pow_1/y*
_output_shapes

:*
T0
Y
Const_10Const*
_output_shapes
:*
valueB"       *
dtype0
[
Sum_3Sumpow_1Const_10*
_output_shapes
: *

Tidx0*
	keep_dims( *
T0
;
add_1AddSum_2Sum_3*
T0*
_output_shapes
: 
R
ArgMax/dimensionConst*
value	B :*
dtype0*
_output_shapes
: 
�
ArgMaxArgMaxNormal/prob/ExpArgMax/dimension*
output_type0	*#
_output_shapes
:���������*

Tidx0*
T0
T
ArgMax_1/dimensionConst*
_output_shapes
: *
value	B :*
dtype0
�
ArgMax_1ArgMaxPlaceholder_3ArgMax_1/dimension*
output_type0	*#
_output_shapes
:���������*

Tidx0*
T0
P
Equal_1EqualArgMaxArgMax_1*
T0	*#
_output_shapes
:���������
d
Cast_1CastEqual_1*

DstT0*#
_output_shapes
:���������*

SrcT0
*
Truncate( 
R
Const_11Const*
dtype0*
_output_shapes
:*
valueB: 
^
Mean_2MeanCast_1Const_11*
T0*
_output_shapes
: *

Tidx0*
	keep_dims( 
L
sub_7/xConst*
dtype0*
_output_shapes
: *
valueB
 *  �?
>
sub_7Subsub_7/xMean_2*
T0*
_output_shapes
: 
[
ones_2Const*
_output_shapes

:*
valueB*  �?*
dtype0
~

Variable_4
VariableV2*
dtype0*
	container *
_output_shapes

:*
shape
:*
shared_name 
�
Variable_4/AssignAssign
Variable_4ones_2*
_class
loc:@Variable_4*
validate_shape(*
_output_shapes

:*
use_locking(*
T0
o
Variable_4/readIdentity
Variable_4*
_class
loc:@Variable_4*
_output_shapes

:*
T0
[
ones_3Const*
valueB*  �?*
dtype0*
_output_shapes

:
~

Variable_5
VariableV2*
shared_name *
dtype0*
	container *
_output_shapes

:*
shape
:
�
Variable_5/AssignAssign
Variable_5ones_3*
use_locking(*
T0*
_class
loc:@Variable_5*
validate_shape(*
_output_shapes

:
o
Variable_5/readIdentity
Variable_5*
T0*
_class
loc:@Variable_5*
_output_shapes

:
p
Placeholder_4Placeholder*
dtype0*'
_output_shapes
:���������*
shape:���������
p
Placeholder_5Placeholder*
shape:���������*
dtype0*'
_output_shapes
:���������
R
Normal_1/locIdentityVariable_4/read*
_output_shapes

:*
T0
T
Normal_1/scaleIdentityVariable_5/read*
_output_shapes

:*
T0
s
Normal_1/prob/standardize/subSubPlaceholder_4Normal_1/loc*
T0*'
_output_shapes
:���������
�
!Normal_1/prob/standardize/truedivRealDivNormal_1/prob/standardize/subNormal_1/scale*'
_output_shapes
:���������*
T0
s
Normal_1/prob/SquareSquare!Normal_1/prob/standardize/truediv*
T0*'
_output_shapes
:���������
X
Normal_1/prob/mul/xConst*
valueB
 *   �*
dtype0*
_output_shapes
: 
u
Normal_1/prob/mulMulNormal_1/prob/mul/xNormal_1/prob/Square*
T0*'
_output_shapes
:���������
Q
Normal_1/prob/LogLogNormal_1/scale*
T0*
_output_shapes

:
X
Normal_1/prob/add/xConst*
valueB
 *�?k?*
dtype0*
_output_shapes
: 
i
Normal_1/prob/addAddNormal_1/prob/add/xNormal_1/prob/Log*
T0*
_output_shapes

:
p
Normal_1/prob/subSubNormal_1/prob/mulNormal_1/prob/add*
T0*'
_output_shapes
:���������
]
Normal_1/prob/ExpExpNormal_1/prob/sub*
T0*'
_output_shapes
:���������
Q
Log_3LogNormal_1/prob/Exp*
T0*'
_output_shapes
:���������
T
mul_3MulPlaceholder_5Log_3*'
_output_shapes
:���������*
T0
Y
Const_12Const*
dtype0*
_output_shapes
:*
valueB"       
[
Sum_4Summul_3Const_12*
T0*
_output_shapes
: *

Tidx0*
	keep_dims( 
4
Neg_2NegSum_4*
_output_shapes
: *
T0
K
Const_13Const*
valueB *
dtype0*
_output_shapes
: 
]
Mean_3MeanNeg_2Const_13*

Tidx0*
	keep_dims( *
T0*
_output_shapes
: 
T
gradients_2/ShapeConst*
_output_shapes
: *
valueB *
dtype0
Z
gradients_2/grad_ys_0Const*
dtype0*
_output_shapes
: *
valueB
 *  �?
u
gradients_2/FillFillgradients_2/Shapegradients_2/grad_ys_0*
_output_shapes
: *
T0*

index_type0
h
%gradients_2/Mean_3_grad/Reshape/shapeConst*
valueB *
dtype0*
_output_shapes
: 
�
gradients_2/Mean_3_grad/ReshapeReshapegradients_2/Fill%gradients_2/Mean_3_grad/Reshape/shape*
T0*
Tshape0*
_output_shapes
: 
`
gradients_2/Mean_3_grad/ConstConst*
_output_shapes
: *
valueB *
dtype0
�
gradients_2/Mean_3_grad/TileTilegradients_2/Mean_3_grad/Reshapegradients_2/Mean_3_grad/Const*
_output_shapes
: *

Tmultiples0*
T0
d
gradients_2/Mean_3_grad/Const_1Const*
_output_shapes
: *
valueB
 *  �?*
dtype0
�
gradients_2/Mean_3_grad/truedivRealDivgradients_2/Mean_3_grad/Tilegradients_2/Mean_3_grad/Const_1*
_output_shapes
: *
T0
c
gradients_2/Neg_2_grad/NegNeggradients_2/Mean_3_grad/truediv*
_output_shapes
: *
T0
u
$gradients_2/Sum_4_grad/Reshape/shapeConst*
valueB"      *
dtype0*
_output_shapes
:
�
gradients_2/Sum_4_grad/ReshapeReshapegradients_2/Neg_2_grad/Neg$gradients_2/Sum_4_grad/Reshape/shape*
_output_shapes

:*
T0*
Tshape0
a
gradients_2/Sum_4_grad/ShapeShapemul_3*
T0*
out_type0*
_output_shapes
:
�
gradients_2/Sum_4_grad/TileTilegradients_2/Sum_4_grad/Reshapegradients_2/Sum_4_grad/Shape*

Tmultiples0*
T0*'
_output_shapes
:���������
i
gradients_2/mul_3_grad/ShapeShapePlaceholder_5*
T0*
out_type0*
_output_shapes
:
c
gradients_2/mul_3_grad/Shape_1ShapeLog_3*
_output_shapes
:*
T0*
out_type0
�
,gradients_2/mul_3_grad/BroadcastGradientArgsBroadcastGradientArgsgradients_2/mul_3_grad/Shapegradients_2/mul_3_grad/Shape_1*2
_output_shapes 
:���������:���������*
T0
w
gradients_2/mul_3_grad/MulMulgradients_2/Sum_4_grad/TileLog_3*
T0*'
_output_shapes
:���������
�
gradients_2/mul_3_grad/SumSumgradients_2/mul_3_grad/Mul,gradients_2/mul_3_grad/BroadcastGradientArgs*
T0*
_output_shapes
:*

Tidx0*
	keep_dims( 
�
gradients_2/mul_3_grad/ReshapeReshapegradients_2/mul_3_grad/Sumgradients_2/mul_3_grad/Shape*'
_output_shapes
:���������*
T0*
Tshape0
�
gradients_2/mul_3_grad/Mul_1MulPlaceholder_5gradients_2/Sum_4_grad/Tile*
T0*'
_output_shapes
:���������
�
gradients_2/mul_3_grad/Sum_1Sumgradients_2/mul_3_grad/Mul_1.gradients_2/mul_3_grad/BroadcastGradientArgs:1*
_output_shapes
:*

Tidx0*
	keep_dims( *
T0
�
 gradients_2/mul_3_grad/Reshape_1Reshapegradients_2/mul_3_grad/Sum_1gradients_2/mul_3_grad/Shape_1*'
_output_shapes
:���������*
T0*
Tshape0
s
'gradients_2/mul_3_grad/tuple/group_depsNoOp^gradients_2/mul_3_grad/Reshape!^gradients_2/mul_3_grad/Reshape_1
�
/gradients_2/mul_3_grad/tuple/control_dependencyIdentitygradients_2/mul_3_grad/Reshape(^gradients_2/mul_3_grad/tuple/group_deps*'
_output_shapes
:���������*
T0*1
_class'
%#loc:@gradients_2/mul_3_grad/Reshape
�
1gradients_2/mul_3_grad/tuple/control_dependency_1Identity gradients_2/mul_3_grad/Reshape_1(^gradients_2/mul_3_grad/tuple/group_deps*
T0*3
_class)
'%loc:@gradients_2/mul_3_grad/Reshape_1*'
_output_shapes
:���������
�
!gradients_2/Log_3_grad/Reciprocal
ReciprocalNormal_1/prob/Exp2^gradients_2/mul_3_grad/tuple/control_dependency_1*'
_output_shapes
:���������*
T0
�
gradients_2/Log_3_grad/mulMul1gradients_2/mul_3_grad/tuple/control_dependency_1!gradients_2/Log_3_grad/Reciprocal*
T0*'
_output_shapes
:���������
�
&gradients_2/Normal_1/prob/Exp_grad/mulMulgradients_2/Log_3_grad/mulNormal_1/prob/Exp*'
_output_shapes
:���������*
T0
y
(gradients_2/Normal_1/prob/sub_grad/ShapeShapeNormal_1/prob/mul*
T0*
out_type0*
_output_shapes
:
{
*gradients_2/Normal_1/prob/sub_grad/Shape_1Const*
valueB"      *
dtype0*
_output_shapes
:
�
8gradients_2/Normal_1/prob/sub_grad/BroadcastGradientArgsBroadcastGradientArgs(gradients_2/Normal_1/prob/sub_grad/Shape*gradients_2/Normal_1/prob/sub_grad/Shape_1*
T0*2
_output_shapes 
:���������:���������
�
&gradients_2/Normal_1/prob/sub_grad/SumSum&gradients_2/Normal_1/prob/Exp_grad/mul8gradients_2/Normal_1/prob/sub_grad/BroadcastGradientArgs*
T0*
_output_shapes
:*

Tidx0*
	keep_dims( 
�
*gradients_2/Normal_1/prob/sub_grad/ReshapeReshape&gradients_2/Normal_1/prob/sub_grad/Sum(gradients_2/Normal_1/prob/sub_grad/Shape*
T0*
Tshape0*'
_output_shapes
:���������
�
(gradients_2/Normal_1/prob/sub_grad/Sum_1Sum&gradients_2/Normal_1/prob/Exp_grad/mul:gradients_2/Normal_1/prob/sub_grad/BroadcastGradientArgs:1*
_output_shapes
:*

Tidx0*
	keep_dims( *
T0
z
&gradients_2/Normal_1/prob/sub_grad/NegNeg(gradients_2/Normal_1/prob/sub_grad/Sum_1*
T0*
_output_shapes
:
�
,gradients_2/Normal_1/prob/sub_grad/Reshape_1Reshape&gradients_2/Normal_1/prob/sub_grad/Neg*gradients_2/Normal_1/prob/sub_grad/Shape_1*
_output_shapes

:*
T0*
Tshape0
�
3gradients_2/Normal_1/prob/sub_grad/tuple/group_depsNoOp+^gradients_2/Normal_1/prob/sub_grad/Reshape-^gradients_2/Normal_1/prob/sub_grad/Reshape_1
�
;gradients_2/Normal_1/prob/sub_grad/tuple/control_dependencyIdentity*gradients_2/Normal_1/prob/sub_grad/Reshape4^gradients_2/Normal_1/prob/sub_grad/tuple/group_deps*
T0*=
_class3
1/loc:@gradients_2/Normal_1/prob/sub_grad/Reshape*'
_output_shapes
:���������
�
=gradients_2/Normal_1/prob/sub_grad/tuple/control_dependency_1Identity,gradients_2/Normal_1/prob/sub_grad/Reshape_14^gradients_2/Normal_1/prob/sub_grad/tuple/group_deps*
_output_shapes

:*
T0*?
_class5
31loc:@gradients_2/Normal_1/prob/sub_grad/Reshape_1
k
(gradients_2/Normal_1/prob/mul_grad/ShapeConst*
_output_shapes
: *
valueB *
dtype0
~
*gradients_2/Normal_1/prob/mul_grad/Shape_1ShapeNormal_1/prob/Square*
_output_shapes
:*
T0*
out_type0
�
8gradients_2/Normal_1/prob/mul_grad/BroadcastGradientArgsBroadcastGradientArgs(gradients_2/Normal_1/prob/mul_grad/Shape*gradients_2/Normal_1/prob/mul_grad/Shape_1*
T0*2
_output_shapes 
:���������:���������
�
&gradients_2/Normal_1/prob/mul_grad/MulMul;gradients_2/Normal_1/prob/sub_grad/tuple/control_dependencyNormal_1/prob/Square*
T0*'
_output_shapes
:���������
�
&gradients_2/Normal_1/prob/mul_grad/SumSum&gradients_2/Normal_1/prob/mul_grad/Mul8gradients_2/Normal_1/prob/mul_grad/BroadcastGradientArgs*
_output_shapes
:*

Tidx0*
	keep_dims( *
T0
�
*gradients_2/Normal_1/prob/mul_grad/ReshapeReshape&gradients_2/Normal_1/prob/mul_grad/Sum(gradients_2/Normal_1/prob/mul_grad/Shape*
T0*
Tshape0*
_output_shapes
: 
�
(gradients_2/Normal_1/prob/mul_grad/Mul_1MulNormal_1/prob/mul/x;gradients_2/Normal_1/prob/sub_grad/tuple/control_dependency*'
_output_shapes
:���������*
T0
�
(gradients_2/Normal_1/prob/mul_grad/Sum_1Sum(gradients_2/Normal_1/prob/mul_grad/Mul_1:gradients_2/Normal_1/prob/mul_grad/BroadcastGradientArgs:1*
T0*
_output_shapes
:*

Tidx0*
	keep_dims( 
�
,gradients_2/Normal_1/prob/mul_grad/Reshape_1Reshape(gradients_2/Normal_1/prob/mul_grad/Sum_1*gradients_2/Normal_1/prob/mul_grad/Shape_1*
Tshape0*'
_output_shapes
:���������*
T0
�
3gradients_2/Normal_1/prob/mul_grad/tuple/group_depsNoOp+^gradients_2/Normal_1/prob/mul_grad/Reshape-^gradients_2/Normal_1/prob/mul_grad/Reshape_1
�
;gradients_2/Normal_1/prob/mul_grad/tuple/control_dependencyIdentity*gradients_2/Normal_1/prob/mul_grad/Reshape4^gradients_2/Normal_1/prob/mul_grad/tuple/group_deps*
_output_shapes
: *
T0*=
_class3
1/loc:@gradients_2/Normal_1/prob/mul_grad/Reshape
�
=gradients_2/Normal_1/prob/mul_grad/tuple/control_dependency_1Identity,gradients_2/Normal_1/prob/mul_grad/Reshape_14^gradients_2/Normal_1/prob/mul_grad/tuple/group_deps*
T0*?
_class5
31loc:@gradients_2/Normal_1/prob/mul_grad/Reshape_1*'
_output_shapes
:���������
k
(gradients_2/Normal_1/prob/add_grad/ShapeConst*
valueB *
dtype0*
_output_shapes
: 
{
*gradients_2/Normal_1/prob/add_grad/Shape_1Const*
valueB"      *
dtype0*
_output_shapes
:
�
8gradients_2/Normal_1/prob/add_grad/BroadcastGradientArgsBroadcastGradientArgs(gradients_2/Normal_1/prob/add_grad/Shape*gradients_2/Normal_1/prob/add_grad/Shape_1*2
_output_shapes 
:���������:���������*
T0
�
&gradients_2/Normal_1/prob/add_grad/SumSum=gradients_2/Normal_1/prob/sub_grad/tuple/control_dependency_18gradients_2/Normal_1/prob/add_grad/BroadcastGradientArgs*

Tidx0*
	keep_dims( *
T0*
_output_shapes
: 
�
*gradients_2/Normal_1/prob/add_grad/ReshapeReshape&gradients_2/Normal_1/prob/add_grad/Sum(gradients_2/Normal_1/prob/add_grad/Shape*
_output_shapes
: *
T0*
Tshape0
�
(gradients_2/Normal_1/prob/add_grad/Sum_1Sum=gradients_2/Normal_1/prob/sub_grad/tuple/control_dependency_1:gradients_2/Normal_1/prob/add_grad/BroadcastGradientArgs:1*
_output_shapes
:*

Tidx0*
	keep_dims( *
T0
�
,gradients_2/Normal_1/prob/add_grad/Reshape_1Reshape(gradients_2/Normal_1/prob/add_grad/Sum_1*gradients_2/Normal_1/prob/add_grad/Shape_1*
T0*
Tshape0*
_output_shapes

:
�
3gradients_2/Normal_1/prob/add_grad/tuple/group_depsNoOp+^gradients_2/Normal_1/prob/add_grad/Reshape-^gradients_2/Normal_1/prob/add_grad/Reshape_1
�
;gradients_2/Normal_1/prob/add_grad/tuple/control_dependencyIdentity*gradients_2/Normal_1/prob/add_grad/Reshape4^gradients_2/Normal_1/prob/add_grad/tuple/group_deps*
_output_shapes
: *
T0*=
_class3
1/loc:@gradients_2/Normal_1/prob/add_grad/Reshape
�
=gradients_2/Normal_1/prob/add_grad/tuple/control_dependency_1Identity,gradients_2/Normal_1/prob/add_grad/Reshape_14^gradients_2/Normal_1/prob/add_grad/tuple/group_deps*
T0*?
_class5
31loc:@gradients_2/Normal_1/prob/add_grad/Reshape_1*
_output_shapes

:
�
+gradients_2/Normal_1/prob/Square_grad/ConstConst>^gradients_2/Normal_1/prob/mul_grad/tuple/control_dependency_1*
valueB
 *   @*
dtype0*
_output_shapes
: 
�
)gradients_2/Normal_1/prob/Square_grad/MulMul!Normal_1/prob/standardize/truediv+gradients_2/Normal_1/prob/Square_grad/Const*'
_output_shapes
:���������*
T0
�
+gradients_2/Normal_1/prob/Square_grad/Mul_1Mul=gradients_2/Normal_1/prob/mul_grad/tuple/control_dependency_1)gradients_2/Normal_1/prob/Square_grad/Mul*'
_output_shapes
:���������*
T0
�
-gradients_2/Normal_1/prob/Log_grad/Reciprocal
ReciprocalNormal_1/scale>^gradients_2/Normal_1/prob/add_grad/tuple/control_dependency_1*
_output_shapes

:*
T0
�
&gradients_2/Normal_1/prob/Log_grad/mulMul=gradients_2/Normal_1/prob/add_grad/tuple/control_dependency_1-gradients_2/Normal_1/prob/Log_grad/Reciprocal*
T0*
_output_shapes

:
�
8gradients_2/Normal_1/prob/standardize/truediv_grad/ShapeShapeNormal_1/prob/standardize/sub*
_output_shapes
:*
T0*
out_type0
�
:gradients_2/Normal_1/prob/standardize/truediv_grad/Shape_1Const*
_output_shapes
:*
valueB"      *
dtype0
�
Hgradients_2/Normal_1/prob/standardize/truediv_grad/BroadcastGradientArgsBroadcastGradientArgs8gradients_2/Normal_1/prob/standardize/truediv_grad/Shape:gradients_2/Normal_1/prob/standardize/truediv_grad/Shape_1*
T0*2
_output_shapes 
:���������:���������
�
:gradients_2/Normal_1/prob/standardize/truediv_grad/RealDivRealDiv+gradients_2/Normal_1/prob/Square_grad/Mul_1Normal_1/scale*'
_output_shapes
:���������*
T0
�
6gradients_2/Normal_1/prob/standardize/truediv_grad/SumSum:gradients_2/Normal_1/prob/standardize/truediv_grad/RealDivHgradients_2/Normal_1/prob/standardize/truediv_grad/BroadcastGradientArgs*
T0*
_output_shapes
:*

Tidx0*
	keep_dims( 
�
:gradients_2/Normal_1/prob/standardize/truediv_grad/ReshapeReshape6gradients_2/Normal_1/prob/standardize/truediv_grad/Sum8gradients_2/Normal_1/prob/standardize/truediv_grad/Shape*
T0*
Tshape0*'
_output_shapes
:���������
�
6gradients_2/Normal_1/prob/standardize/truediv_grad/NegNegNormal_1/prob/standardize/sub*'
_output_shapes
:���������*
T0
�
<gradients_2/Normal_1/prob/standardize/truediv_grad/RealDiv_1RealDiv6gradients_2/Normal_1/prob/standardize/truediv_grad/NegNormal_1/scale*'
_output_shapes
:���������*
T0
�
<gradients_2/Normal_1/prob/standardize/truediv_grad/RealDiv_2RealDiv<gradients_2/Normal_1/prob/standardize/truediv_grad/RealDiv_1Normal_1/scale*
T0*'
_output_shapes
:���������
�
6gradients_2/Normal_1/prob/standardize/truediv_grad/mulMul+gradients_2/Normal_1/prob/Square_grad/Mul_1<gradients_2/Normal_1/prob/standardize/truediv_grad/RealDiv_2*'
_output_shapes
:���������*
T0
�
8gradients_2/Normal_1/prob/standardize/truediv_grad/Sum_1Sum6gradients_2/Normal_1/prob/standardize/truediv_grad/mulJgradients_2/Normal_1/prob/standardize/truediv_grad/BroadcastGradientArgs:1*
_output_shapes
:*

Tidx0*
	keep_dims( *
T0
�
<gradients_2/Normal_1/prob/standardize/truediv_grad/Reshape_1Reshape8gradients_2/Normal_1/prob/standardize/truediv_grad/Sum_1:gradients_2/Normal_1/prob/standardize/truediv_grad/Shape_1*
T0*
Tshape0*
_output_shapes

:
�
Cgradients_2/Normal_1/prob/standardize/truediv_grad/tuple/group_depsNoOp;^gradients_2/Normal_1/prob/standardize/truediv_grad/Reshape=^gradients_2/Normal_1/prob/standardize/truediv_grad/Reshape_1
�
Kgradients_2/Normal_1/prob/standardize/truediv_grad/tuple/control_dependencyIdentity:gradients_2/Normal_1/prob/standardize/truediv_grad/ReshapeD^gradients_2/Normal_1/prob/standardize/truediv_grad/tuple/group_deps*M
_classC
A?loc:@gradients_2/Normal_1/prob/standardize/truediv_grad/Reshape*'
_output_shapes
:���������*
T0
�
Mgradients_2/Normal_1/prob/standardize/truediv_grad/tuple/control_dependency_1Identity<gradients_2/Normal_1/prob/standardize/truediv_grad/Reshape_1D^gradients_2/Normal_1/prob/standardize/truediv_grad/tuple/group_deps*O
_classE
CAloc:@gradients_2/Normal_1/prob/standardize/truediv_grad/Reshape_1*
_output_shapes

:*
T0
�
4gradients_2/Normal_1/prob/standardize/sub_grad/ShapeShapePlaceholder_4*
out_type0*
_output_shapes
:*
T0
�
6gradients_2/Normal_1/prob/standardize/sub_grad/Shape_1Const*
dtype0*
_output_shapes
:*
valueB"      
�
Dgradients_2/Normal_1/prob/standardize/sub_grad/BroadcastGradientArgsBroadcastGradientArgs4gradients_2/Normal_1/prob/standardize/sub_grad/Shape6gradients_2/Normal_1/prob/standardize/sub_grad/Shape_1*
T0*2
_output_shapes 
:���������:���������
�
2gradients_2/Normal_1/prob/standardize/sub_grad/SumSumKgradients_2/Normal_1/prob/standardize/truediv_grad/tuple/control_dependencyDgradients_2/Normal_1/prob/standardize/sub_grad/BroadcastGradientArgs*
_output_shapes
:*

Tidx0*
	keep_dims( *
T0
�
6gradients_2/Normal_1/prob/standardize/sub_grad/ReshapeReshape2gradients_2/Normal_1/prob/standardize/sub_grad/Sum4gradients_2/Normal_1/prob/standardize/sub_grad/Shape*
Tshape0*'
_output_shapes
:���������*
T0
�
4gradients_2/Normal_1/prob/standardize/sub_grad/Sum_1SumKgradients_2/Normal_1/prob/standardize/truediv_grad/tuple/control_dependencyFgradients_2/Normal_1/prob/standardize/sub_grad/BroadcastGradientArgs:1*

Tidx0*
	keep_dims( *
T0*
_output_shapes
:
�
2gradients_2/Normal_1/prob/standardize/sub_grad/NegNeg4gradients_2/Normal_1/prob/standardize/sub_grad/Sum_1*
T0*
_output_shapes
:
�
8gradients_2/Normal_1/prob/standardize/sub_grad/Reshape_1Reshape2gradients_2/Normal_1/prob/standardize/sub_grad/Neg6gradients_2/Normal_1/prob/standardize/sub_grad/Shape_1*
T0*
Tshape0*
_output_shapes

:
�
?gradients_2/Normal_1/prob/standardize/sub_grad/tuple/group_depsNoOp7^gradients_2/Normal_1/prob/standardize/sub_grad/Reshape9^gradients_2/Normal_1/prob/standardize/sub_grad/Reshape_1
�
Ggradients_2/Normal_1/prob/standardize/sub_grad/tuple/control_dependencyIdentity6gradients_2/Normal_1/prob/standardize/sub_grad/Reshape@^gradients_2/Normal_1/prob/standardize/sub_grad/tuple/group_deps*'
_output_shapes
:���������*
T0*I
_class?
=;loc:@gradients_2/Normal_1/prob/standardize/sub_grad/Reshape
�
Igradients_2/Normal_1/prob/standardize/sub_grad/tuple/control_dependency_1Identity8gradients_2/Normal_1/prob/standardize/sub_grad/Reshape_1@^gradients_2/Normal_1/prob/standardize/sub_grad/tuple/group_deps*K
_classA
?=loc:@gradients_2/Normal_1/prob/standardize/sub_grad/Reshape_1*
_output_shapes

:*
T0
�
gradients_2/AddNAddN&gradients_2/Normal_1/prob/Log_grad/mulMgradients_2/Normal_1/prob/standardize/truediv_grad/tuple/control_dependency_1*
T0*9
_class/
-+loc:@gradients_2/Normal_1/prob/Log_grad/mul*
N*
_output_shapes

:
�
$Variable_4/Adagrad/Initializer/ConstConst*
valueB*���=*
_class
loc:@Variable_4*
dtype0*
_output_shapes

:
�
Variable_4/Adagrad
VariableV2*
shape
:*
dtype0*
_output_shapes

:*
shared_name *
_class
loc:@Variable_4*
	container 
�
Variable_4/Adagrad/AssignAssignVariable_4/Adagrad$Variable_4/Adagrad/Initializer/Const*
validate_shape(*
_output_shapes

:*
use_locking(*
T0*
_class
loc:@Variable_4

Variable_4/Adagrad/readIdentityVariable_4/Adagrad*
T0*
_class
loc:@Variable_4*
_output_shapes

:
�
$Variable_5/Adagrad/Initializer/ConstConst*
valueB*���=*
_class
loc:@Variable_5*
dtype0*
_output_shapes

:
�
Variable_5/Adagrad
VariableV2*
dtype0*
_output_shapes

:*
shared_name *
_class
loc:@Variable_5*
	container *
shape
:
�
Variable_5/Adagrad/AssignAssignVariable_5/Adagrad$Variable_5/Adagrad/Initializer/Const*
use_locking(*
T0*
_class
loc:@Variable_5*
validate_shape(*
_output_shapes

:

Variable_5/Adagrad/readIdentityVariable_5/Adagrad*
T0*
_class
loc:@Variable_5*
_output_shapes

:
\
Adagrad_1/learning_rateConst*
_output_shapes
: *
valueB
 *ff�>*
dtype0
�
(Adagrad_1/update_Variable_4/ApplyAdagradApplyAdagrad
Variable_4Variable_4/AdagradAdagrad_1/learning_rateIgradients_2/Normal_1/prob/standardize/sub_grad/tuple/control_dependency_1*
use_locking( *
T0*
_class
loc:@Variable_4*
update_slots(*
_output_shapes

:
�
(Adagrad_1/update_Variable_5/ApplyAdagradApplyAdagrad
Variable_5Variable_5/AdagradAdagrad_1/learning_rategradients_2/AddN*
use_locking( *
T0*
_class
loc:@Variable_5*
update_slots(*
_output_shapes

:
g
	Adagrad_1NoOp)^Adagrad_1/update_Variable_4/ApplyAdagrad)^Adagrad_1/update_Variable_5/ApplyAdagrad
�
init_2NoOp^Variable/Assign^Variable_1/Assign^Variable_2/Adagrad/Assign^Variable_2/Assign^Variable_3/Adagrad/Assign^Variable_3/Assign^Variable_4/Adagrad/Assign^Variable_4/Assign^Variable_5/Adagrad/Assign^Variable_5/Assign
T
ArgMax_2/dimensionConst*
_output_shapes
: *
value	B :*
dtype0
�
ArgMax_2ArgMaxNormal_1/prob/ExpArgMax_2/dimension*
T0*
output_type0	*#
_output_shapes
:���������*

Tidx0
[
ones_4Const*
valueB*  �?*
dtype0*
_output_shapes

:
~

Variable_6
VariableV2*
dtype0*
	container *
_output_shapes

:*
shape
:*
shared_name 
�
Variable_6/AssignAssign
Variable_6ones_4*
_class
loc:@Variable_6*
validate_shape(*
_output_shapes

:*
use_locking(*
T0
o
Variable_6/readIdentity
Variable_6*
T0*
_class
loc:@Variable_6*
_output_shapes

:
[
ones_5Const*
valueB*  �?*
dtype0*
_output_shapes

:
~

Variable_7
VariableV2*
shared_name *
dtype0*
	container *
_output_shapes

:*
shape
:
�
Variable_7/AssignAssign
Variable_7ones_5*
_class
loc:@Variable_7*
validate_shape(*
_output_shapes

:*
use_locking(*
T0
o
Variable_7/readIdentity
Variable_7*
_output_shapes

:*
T0*
_class
loc:@Variable_7
p
Placeholder_6Placeholder*
dtype0*'
_output_shapes
:���������*
shape:���������
p
Placeholder_7Placeholder*'
_output_shapes
:���������*
shape:���������*
dtype0
R
Normal_2/locIdentityVariable_6/read*
_output_shapes

:*
T0
T
Normal_2/scaleIdentityVariable_7/read*
_output_shapes

:*
T0
s
Normal_2/prob/standardize/subSubPlaceholder_6Normal_2/loc*
T0*'
_output_shapes
:���������
�
!Normal_2/prob/standardize/truedivRealDivNormal_2/prob/standardize/subNormal_2/scale*
T0*'
_output_shapes
:���������
s
Normal_2/prob/SquareSquare!Normal_2/prob/standardize/truediv*
T0*'
_output_shapes
:���������
X
Normal_2/prob/mul/xConst*
valueB
 *   �*
dtype0*
_output_shapes
: 
u
Normal_2/prob/mulMulNormal_2/prob/mul/xNormal_2/prob/Square*
T0*'
_output_shapes
:���������
Q
Normal_2/prob/LogLogNormal_2/scale*
T0*
_output_shapes

:
X
Normal_2/prob/add/xConst*
dtype0*
_output_shapes
: *
valueB
 *�?k?
i
Normal_2/prob/addAddNormal_2/prob/add/xNormal_2/prob/Log*
_output_shapes

:*
T0
p
Normal_2/prob/subSubNormal_2/prob/mulNormal_2/prob/add*'
_output_shapes
:���������*
T0
]
Normal_2/prob/ExpExpNormal_2/prob/sub*
T0*'
_output_shapes
:���������
Q
Log_4LogNormal_2/prob/Exp*
T0*'
_output_shapes
:���������
T
mul_4MulPlaceholder_7Log_4*'
_output_shapes
:���������*
T0
Y
Const_14Const*
valueB"       *
dtype0*
_output_shapes
:
[
Sum_5Summul_4Const_14*
T0*
_output_shapes
: *

Tidx0*
	keep_dims( 
4
Neg_3NegSum_5*
T0*
_output_shapes
: 
K
Const_15Const*
valueB *
dtype0*
_output_shapes
: 
]
Mean_4MeanNeg_3Const_15*
_output_shapes
: *

Tidx0*
	keep_dims( *
T0
T
gradients_3/ShapeConst*
dtype0*
_output_shapes
: *
valueB 
Z
gradients_3/grad_ys_0Const*
valueB
 *  �?*
dtype0*
_output_shapes
: 
u
gradients_3/FillFillgradients_3/Shapegradients_3/grad_ys_0*
T0*

index_type0*
_output_shapes
: 
h
%gradients_3/Mean_4_grad/Reshape/shapeConst*
valueB *
dtype0*
_output_shapes
: 
�
gradients_3/Mean_4_grad/ReshapeReshapegradients_3/Fill%gradients_3/Mean_4_grad/Reshape/shape*
_output_shapes
: *
T0*
Tshape0
`
gradients_3/Mean_4_grad/ConstConst*
valueB *
dtype0*
_output_shapes
: 
�
gradients_3/Mean_4_grad/TileTilegradients_3/Mean_4_grad/Reshapegradients_3/Mean_4_grad/Const*

Tmultiples0*
T0*
_output_shapes
: 
d
gradients_3/Mean_4_grad/Const_1Const*
valueB
 *  �?*
dtype0*
_output_shapes
: 
�
gradients_3/Mean_4_grad/truedivRealDivgradients_3/Mean_4_grad/Tilegradients_3/Mean_4_grad/Const_1*
T0*
_output_shapes
: 
c
gradients_3/Neg_3_grad/NegNeggradients_3/Mean_4_grad/truediv*
_output_shapes
: *
T0
u
$gradients_3/Sum_5_grad/Reshape/shapeConst*
valueB"      *
dtype0*
_output_shapes
:
�
gradients_3/Sum_5_grad/ReshapeReshapegradients_3/Neg_3_grad/Neg$gradients_3/Sum_5_grad/Reshape/shape*
T0*
Tshape0*
_output_shapes

:
a
gradients_3/Sum_5_grad/ShapeShapemul_4*
_output_shapes
:*
T0*
out_type0
�
gradients_3/Sum_5_grad/TileTilegradients_3/Sum_5_grad/Reshapegradients_3/Sum_5_grad/Shape*'
_output_shapes
:���������*

Tmultiples0*
T0
i
gradients_3/mul_4_grad/ShapeShapePlaceholder_7*
_output_shapes
:*
T0*
out_type0
c
gradients_3/mul_4_grad/Shape_1ShapeLog_4*
T0*
out_type0*
_output_shapes
:
�
,gradients_3/mul_4_grad/BroadcastGradientArgsBroadcastGradientArgsgradients_3/mul_4_grad/Shapegradients_3/mul_4_grad/Shape_1*2
_output_shapes 
:���������:���������*
T0
w
gradients_3/mul_4_grad/MulMulgradients_3/Sum_5_grad/TileLog_4*'
_output_shapes
:���������*
T0
�
gradients_3/mul_4_grad/SumSumgradients_3/mul_4_grad/Mul,gradients_3/mul_4_grad/BroadcastGradientArgs*
_output_shapes
:*

Tidx0*
	keep_dims( *
T0
�
gradients_3/mul_4_grad/ReshapeReshapegradients_3/mul_4_grad/Sumgradients_3/mul_4_grad/Shape*
T0*
Tshape0*'
_output_shapes
:���������
�
gradients_3/mul_4_grad/Mul_1MulPlaceholder_7gradients_3/Sum_5_grad/Tile*
T0*'
_output_shapes
:���������
�
gradients_3/mul_4_grad/Sum_1Sumgradients_3/mul_4_grad/Mul_1.gradients_3/mul_4_grad/BroadcastGradientArgs:1*
_output_shapes
:*

Tidx0*
	keep_dims( *
T0
�
 gradients_3/mul_4_grad/Reshape_1Reshapegradients_3/mul_4_grad/Sum_1gradients_3/mul_4_grad/Shape_1*'
_output_shapes
:���������*
T0*
Tshape0
s
'gradients_3/mul_4_grad/tuple/group_depsNoOp^gradients_3/mul_4_grad/Reshape!^gradients_3/mul_4_grad/Reshape_1
�
/gradients_3/mul_4_grad/tuple/control_dependencyIdentitygradients_3/mul_4_grad/Reshape(^gradients_3/mul_4_grad/tuple/group_deps*1
_class'
%#loc:@gradients_3/mul_4_grad/Reshape*'
_output_shapes
:���������*
T0
�
1gradients_3/mul_4_grad/tuple/control_dependency_1Identity gradients_3/mul_4_grad/Reshape_1(^gradients_3/mul_4_grad/tuple/group_deps*
T0*3
_class)
'%loc:@gradients_3/mul_4_grad/Reshape_1*'
_output_shapes
:���������
�
!gradients_3/Log_4_grad/Reciprocal
ReciprocalNormal_2/prob/Exp2^gradients_3/mul_4_grad/tuple/control_dependency_1*'
_output_shapes
:���������*
T0
�
gradients_3/Log_4_grad/mulMul1gradients_3/mul_4_grad/tuple/control_dependency_1!gradients_3/Log_4_grad/Reciprocal*'
_output_shapes
:���������*
T0
�
&gradients_3/Normal_2/prob/Exp_grad/mulMulgradients_3/Log_4_grad/mulNormal_2/prob/Exp*
T0*'
_output_shapes
:���������
y
(gradients_3/Normal_2/prob/sub_grad/ShapeShapeNormal_2/prob/mul*
T0*
out_type0*
_output_shapes
:
{
*gradients_3/Normal_2/prob/sub_grad/Shape_1Const*
_output_shapes
:*
valueB"      *
dtype0
�
8gradients_3/Normal_2/prob/sub_grad/BroadcastGradientArgsBroadcastGradientArgs(gradients_3/Normal_2/prob/sub_grad/Shape*gradients_3/Normal_2/prob/sub_grad/Shape_1*
T0*2
_output_shapes 
:���������:���������
�
&gradients_3/Normal_2/prob/sub_grad/SumSum&gradients_3/Normal_2/prob/Exp_grad/mul8gradients_3/Normal_2/prob/sub_grad/BroadcastGradientArgs*
_output_shapes
:*

Tidx0*
	keep_dims( *
T0
�
*gradients_3/Normal_2/prob/sub_grad/ReshapeReshape&gradients_3/Normal_2/prob/sub_grad/Sum(gradients_3/Normal_2/prob/sub_grad/Shape*
T0*
Tshape0*'
_output_shapes
:���������
�
(gradients_3/Normal_2/prob/sub_grad/Sum_1Sum&gradients_3/Normal_2/prob/Exp_grad/mul:gradients_3/Normal_2/prob/sub_grad/BroadcastGradientArgs:1*
T0*
_output_shapes
:*

Tidx0*
	keep_dims( 
z
&gradients_3/Normal_2/prob/sub_grad/NegNeg(gradients_3/Normal_2/prob/sub_grad/Sum_1*
_output_shapes
:*
T0
�
,gradients_3/Normal_2/prob/sub_grad/Reshape_1Reshape&gradients_3/Normal_2/prob/sub_grad/Neg*gradients_3/Normal_2/prob/sub_grad/Shape_1*
T0*
Tshape0*
_output_shapes

:
�
3gradients_3/Normal_2/prob/sub_grad/tuple/group_depsNoOp+^gradients_3/Normal_2/prob/sub_grad/Reshape-^gradients_3/Normal_2/prob/sub_grad/Reshape_1
�
;gradients_3/Normal_2/prob/sub_grad/tuple/control_dependencyIdentity*gradients_3/Normal_2/prob/sub_grad/Reshape4^gradients_3/Normal_2/prob/sub_grad/tuple/group_deps*'
_output_shapes
:���������*
T0*=
_class3
1/loc:@gradients_3/Normal_2/prob/sub_grad/Reshape
�
=gradients_3/Normal_2/prob/sub_grad/tuple/control_dependency_1Identity,gradients_3/Normal_2/prob/sub_grad/Reshape_14^gradients_3/Normal_2/prob/sub_grad/tuple/group_deps*
T0*?
_class5
31loc:@gradients_3/Normal_2/prob/sub_grad/Reshape_1*
_output_shapes

:
k
(gradients_3/Normal_2/prob/mul_grad/ShapeConst*
dtype0*
_output_shapes
: *
valueB 
~
*gradients_3/Normal_2/prob/mul_grad/Shape_1ShapeNormal_2/prob/Square*
_output_shapes
:*
T0*
out_type0
�
8gradients_3/Normal_2/prob/mul_grad/BroadcastGradientArgsBroadcastGradientArgs(gradients_3/Normal_2/prob/mul_grad/Shape*gradients_3/Normal_2/prob/mul_grad/Shape_1*
T0*2
_output_shapes 
:���������:���������
�
&gradients_3/Normal_2/prob/mul_grad/MulMul;gradients_3/Normal_2/prob/sub_grad/tuple/control_dependencyNormal_2/prob/Square*
T0*'
_output_shapes
:���������
�
&gradients_3/Normal_2/prob/mul_grad/SumSum&gradients_3/Normal_2/prob/mul_grad/Mul8gradients_3/Normal_2/prob/mul_grad/BroadcastGradientArgs*
_output_shapes
:*

Tidx0*
	keep_dims( *
T0
�
*gradients_3/Normal_2/prob/mul_grad/ReshapeReshape&gradients_3/Normal_2/prob/mul_grad/Sum(gradients_3/Normal_2/prob/mul_grad/Shape*
Tshape0*
_output_shapes
: *
T0
�
(gradients_3/Normal_2/prob/mul_grad/Mul_1MulNormal_2/prob/mul/x;gradients_3/Normal_2/prob/sub_grad/tuple/control_dependency*'
_output_shapes
:���������*
T0
�
(gradients_3/Normal_2/prob/mul_grad/Sum_1Sum(gradients_3/Normal_2/prob/mul_grad/Mul_1:gradients_3/Normal_2/prob/mul_grad/BroadcastGradientArgs:1*

Tidx0*
	keep_dims( *
T0*
_output_shapes
:
�
,gradients_3/Normal_2/prob/mul_grad/Reshape_1Reshape(gradients_3/Normal_2/prob/mul_grad/Sum_1*gradients_3/Normal_2/prob/mul_grad/Shape_1*
T0*
Tshape0*'
_output_shapes
:���������
�
3gradients_3/Normal_2/prob/mul_grad/tuple/group_depsNoOp+^gradients_3/Normal_2/prob/mul_grad/Reshape-^gradients_3/Normal_2/prob/mul_grad/Reshape_1
�
;gradients_3/Normal_2/prob/mul_grad/tuple/control_dependencyIdentity*gradients_3/Normal_2/prob/mul_grad/Reshape4^gradients_3/Normal_2/prob/mul_grad/tuple/group_deps*
T0*=
_class3
1/loc:@gradients_3/Normal_2/prob/mul_grad/Reshape*
_output_shapes
: 
�
=gradients_3/Normal_2/prob/mul_grad/tuple/control_dependency_1Identity,gradients_3/Normal_2/prob/mul_grad/Reshape_14^gradients_3/Normal_2/prob/mul_grad/tuple/group_deps*'
_output_shapes
:���������*
T0*?
_class5
31loc:@gradients_3/Normal_2/prob/mul_grad/Reshape_1
k
(gradients_3/Normal_2/prob/add_grad/ShapeConst*
valueB *
dtype0*
_output_shapes
: 
{
*gradients_3/Normal_2/prob/add_grad/Shape_1Const*
valueB"      *
dtype0*
_output_shapes
:
�
8gradients_3/Normal_2/prob/add_grad/BroadcastGradientArgsBroadcastGradientArgs(gradients_3/Normal_2/prob/add_grad/Shape*gradients_3/Normal_2/prob/add_grad/Shape_1*
T0*2
_output_shapes 
:���������:���������
�
&gradients_3/Normal_2/prob/add_grad/SumSum=gradients_3/Normal_2/prob/sub_grad/tuple/control_dependency_18gradients_3/Normal_2/prob/add_grad/BroadcastGradientArgs*
T0*
_output_shapes
: *

Tidx0*
	keep_dims( 
�
*gradients_3/Normal_2/prob/add_grad/ReshapeReshape&gradients_3/Normal_2/prob/add_grad/Sum(gradients_3/Normal_2/prob/add_grad/Shape*
_output_shapes
: *
T0*
Tshape0
�
(gradients_3/Normal_2/prob/add_grad/Sum_1Sum=gradients_3/Normal_2/prob/sub_grad/tuple/control_dependency_1:gradients_3/Normal_2/prob/add_grad/BroadcastGradientArgs:1*
_output_shapes
:*

Tidx0*
	keep_dims( *
T0
�
,gradients_3/Normal_2/prob/add_grad/Reshape_1Reshape(gradients_3/Normal_2/prob/add_grad/Sum_1*gradients_3/Normal_2/prob/add_grad/Shape_1*
T0*
Tshape0*
_output_shapes

:
�
3gradients_3/Normal_2/prob/add_grad/tuple/group_depsNoOp+^gradients_3/Normal_2/prob/add_grad/Reshape-^gradients_3/Normal_2/prob/add_grad/Reshape_1
�
;gradients_3/Normal_2/prob/add_grad/tuple/control_dependencyIdentity*gradients_3/Normal_2/prob/add_grad/Reshape4^gradients_3/Normal_2/prob/add_grad/tuple/group_deps*
T0*=
_class3
1/loc:@gradients_3/Normal_2/prob/add_grad/Reshape*
_output_shapes
: 
�
=gradients_3/Normal_2/prob/add_grad/tuple/control_dependency_1Identity,gradients_3/Normal_2/prob/add_grad/Reshape_14^gradients_3/Normal_2/prob/add_grad/tuple/group_deps*
T0*?
_class5
31loc:@gradients_3/Normal_2/prob/add_grad/Reshape_1*
_output_shapes

:
�
+gradients_3/Normal_2/prob/Square_grad/ConstConst>^gradients_3/Normal_2/prob/mul_grad/tuple/control_dependency_1*
valueB
 *   @*
dtype0*
_output_shapes
: 
�
)gradients_3/Normal_2/prob/Square_grad/MulMul!Normal_2/prob/standardize/truediv+gradients_3/Normal_2/prob/Square_grad/Const*'
_output_shapes
:���������*
T0
�
+gradients_3/Normal_2/prob/Square_grad/Mul_1Mul=gradients_3/Normal_2/prob/mul_grad/tuple/control_dependency_1)gradients_3/Normal_2/prob/Square_grad/Mul*'
_output_shapes
:���������*
T0
�
-gradients_3/Normal_2/prob/Log_grad/Reciprocal
ReciprocalNormal_2/scale>^gradients_3/Normal_2/prob/add_grad/tuple/control_dependency_1*
T0*
_output_shapes

:
�
&gradients_3/Normal_2/prob/Log_grad/mulMul=gradients_3/Normal_2/prob/add_grad/tuple/control_dependency_1-gradients_3/Normal_2/prob/Log_grad/Reciprocal*
_output_shapes

:*
T0
�
8gradients_3/Normal_2/prob/standardize/truediv_grad/ShapeShapeNormal_2/prob/standardize/sub*
T0*
out_type0*
_output_shapes
:
�
:gradients_3/Normal_2/prob/standardize/truediv_grad/Shape_1Const*
valueB"      *
dtype0*
_output_shapes
:
�
Hgradients_3/Normal_2/prob/standardize/truediv_grad/BroadcastGradientArgsBroadcastGradientArgs8gradients_3/Normal_2/prob/standardize/truediv_grad/Shape:gradients_3/Normal_2/prob/standardize/truediv_grad/Shape_1*2
_output_shapes 
:���������:���������*
T0
�
:gradients_3/Normal_2/prob/standardize/truediv_grad/RealDivRealDiv+gradients_3/Normal_2/prob/Square_grad/Mul_1Normal_2/scale*
T0*'
_output_shapes
:���������
�
6gradients_3/Normal_2/prob/standardize/truediv_grad/SumSum:gradients_3/Normal_2/prob/standardize/truediv_grad/RealDivHgradients_3/Normal_2/prob/standardize/truediv_grad/BroadcastGradientArgs*
_output_shapes
:*

Tidx0*
	keep_dims( *
T0
�
:gradients_3/Normal_2/prob/standardize/truediv_grad/ReshapeReshape6gradients_3/Normal_2/prob/standardize/truediv_grad/Sum8gradients_3/Normal_2/prob/standardize/truediv_grad/Shape*'
_output_shapes
:���������*
T0*
Tshape0
�
6gradients_3/Normal_2/prob/standardize/truediv_grad/NegNegNormal_2/prob/standardize/sub*'
_output_shapes
:���������*
T0
�
<gradients_3/Normal_2/prob/standardize/truediv_grad/RealDiv_1RealDiv6gradients_3/Normal_2/prob/standardize/truediv_grad/NegNormal_2/scale*
T0*'
_output_shapes
:���������
�
<gradients_3/Normal_2/prob/standardize/truediv_grad/RealDiv_2RealDiv<gradients_3/Normal_2/prob/standardize/truediv_grad/RealDiv_1Normal_2/scale*
T0*'
_output_shapes
:���������
�
6gradients_3/Normal_2/prob/standardize/truediv_grad/mulMul+gradients_3/Normal_2/prob/Square_grad/Mul_1<gradients_3/Normal_2/prob/standardize/truediv_grad/RealDiv_2*
T0*'
_output_shapes
:���������
�
8gradients_3/Normal_2/prob/standardize/truediv_grad/Sum_1Sum6gradients_3/Normal_2/prob/standardize/truediv_grad/mulJgradients_3/Normal_2/prob/standardize/truediv_grad/BroadcastGradientArgs:1*
T0*
_output_shapes
:*

Tidx0*
	keep_dims( 
�
<gradients_3/Normal_2/prob/standardize/truediv_grad/Reshape_1Reshape8gradients_3/Normal_2/prob/standardize/truediv_grad/Sum_1:gradients_3/Normal_2/prob/standardize/truediv_grad/Shape_1*
Tshape0*
_output_shapes

:*
T0
�
Cgradients_3/Normal_2/prob/standardize/truediv_grad/tuple/group_depsNoOp;^gradients_3/Normal_2/prob/standardize/truediv_grad/Reshape=^gradients_3/Normal_2/prob/standardize/truediv_grad/Reshape_1
�
Kgradients_3/Normal_2/prob/standardize/truediv_grad/tuple/control_dependencyIdentity:gradients_3/Normal_2/prob/standardize/truediv_grad/ReshapeD^gradients_3/Normal_2/prob/standardize/truediv_grad/tuple/group_deps*'
_output_shapes
:���������*
T0*M
_classC
A?loc:@gradients_3/Normal_2/prob/standardize/truediv_grad/Reshape
�
Mgradients_3/Normal_2/prob/standardize/truediv_grad/tuple/control_dependency_1Identity<gradients_3/Normal_2/prob/standardize/truediv_grad/Reshape_1D^gradients_3/Normal_2/prob/standardize/truediv_grad/tuple/group_deps*
T0*O
_classE
CAloc:@gradients_3/Normal_2/prob/standardize/truediv_grad/Reshape_1*
_output_shapes

:
�
4gradients_3/Normal_2/prob/standardize/sub_grad/ShapeShapePlaceholder_6*
out_type0*
_output_shapes
:*
T0
�
6gradients_3/Normal_2/prob/standardize/sub_grad/Shape_1Const*
_output_shapes
:*
valueB"      *
dtype0
�
Dgradients_3/Normal_2/prob/standardize/sub_grad/BroadcastGradientArgsBroadcastGradientArgs4gradients_3/Normal_2/prob/standardize/sub_grad/Shape6gradients_3/Normal_2/prob/standardize/sub_grad/Shape_1*
T0*2
_output_shapes 
:���������:���������
�
2gradients_3/Normal_2/prob/standardize/sub_grad/SumSumKgradients_3/Normal_2/prob/standardize/truediv_grad/tuple/control_dependencyDgradients_3/Normal_2/prob/standardize/sub_grad/BroadcastGradientArgs*

Tidx0*
	keep_dims( *
T0*
_output_shapes
:
�
6gradients_3/Normal_2/prob/standardize/sub_grad/ReshapeReshape2gradients_3/Normal_2/prob/standardize/sub_grad/Sum4gradients_3/Normal_2/prob/standardize/sub_grad/Shape*
T0*
Tshape0*'
_output_shapes
:���������
�
4gradients_3/Normal_2/prob/standardize/sub_grad/Sum_1SumKgradients_3/Normal_2/prob/standardize/truediv_grad/tuple/control_dependencyFgradients_3/Normal_2/prob/standardize/sub_grad/BroadcastGradientArgs:1*
T0*
_output_shapes
:*

Tidx0*
	keep_dims( 
�
2gradients_3/Normal_2/prob/standardize/sub_grad/NegNeg4gradients_3/Normal_2/prob/standardize/sub_grad/Sum_1*
T0*
_output_shapes
:
�
8gradients_3/Normal_2/prob/standardize/sub_grad/Reshape_1Reshape2gradients_3/Normal_2/prob/standardize/sub_grad/Neg6gradients_3/Normal_2/prob/standardize/sub_grad/Shape_1*
Tshape0*
_output_shapes

:*
T0
�
?gradients_3/Normal_2/prob/standardize/sub_grad/tuple/group_depsNoOp7^gradients_3/Normal_2/prob/standardize/sub_grad/Reshape9^gradients_3/Normal_2/prob/standardize/sub_grad/Reshape_1
�
Ggradients_3/Normal_2/prob/standardize/sub_grad/tuple/control_dependencyIdentity6gradients_3/Normal_2/prob/standardize/sub_grad/Reshape@^gradients_3/Normal_2/prob/standardize/sub_grad/tuple/group_deps*
T0*I
_class?
=;loc:@gradients_3/Normal_2/prob/standardize/sub_grad/Reshape*'
_output_shapes
:���������
�
Igradients_3/Normal_2/prob/standardize/sub_grad/tuple/control_dependency_1Identity8gradients_3/Normal_2/prob/standardize/sub_grad/Reshape_1@^gradients_3/Normal_2/prob/standardize/sub_grad/tuple/group_deps*K
_classA
?=loc:@gradients_3/Normal_2/prob/standardize/sub_grad/Reshape_1*
_output_shapes

:*
T0
�
gradients_3/AddNAddN&gradients_3/Normal_2/prob/Log_grad/mulMgradients_3/Normal_2/prob/standardize/truediv_grad/tuple/control_dependency_1*
_output_shapes

:*
T0*9
_class/
-+loc:@gradients_3/Normal_2/prob/Log_grad/mul*
N
�
$Variable_6/Adagrad/Initializer/ConstConst*
_output_shapes

:*
valueB*���=*
_class
loc:@Variable_6*
dtype0
�
Variable_6/Adagrad
VariableV2*
shared_name *
_class
loc:@Variable_6*
	container *
shape
:*
dtype0*
_output_shapes

:
�
Variable_6/Adagrad/AssignAssignVariable_6/Adagrad$Variable_6/Adagrad/Initializer/Const*
use_locking(*
T0*
_class
loc:@Variable_6*
validate_shape(*
_output_shapes

:

Variable_6/Adagrad/readIdentityVariable_6/Adagrad*
T0*
_class
loc:@Variable_6*
_output_shapes

:
�
$Variable_7/Adagrad/Initializer/ConstConst*
valueB*���=*
_class
loc:@Variable_7*
dtype0*
_output_shapes

:
�
Variable_7/Adagrad
VariableV2*
	container *
shape
:*
dtype0*
_output_shapes

:*
shared_name *
_class
loc:@Variable_7
�
Variable_7/Adagrad/AssignAssignVariable_7/Adagrad$Variable_7/Adagrad/Initializer/Const*
validate_shape(*
_output_shapes

:*
use_locking(*
T0*
_class
loc:@Variable_7

Variable_7/Adagrad/readIdentityVariable_7/Adagrad*
T0*
_class
loc:@Variable_7*
_output_shapes

:
\
Adagrad_2/learning_rateConst*
dtype0*
_output_shapes
: *
valueB
 *ff�>
�
(Adagrad_2/update_Variable_6/ApplyAdagradApplyAdagrad
Variable_6Variable_6/AdagradAdagrad_2/learning_rateIgradients_3/Normal_2/prob/standardize/sub_grad/tuple/control_dependency_1*
use_locking( *
T0*
_class
loc:@Variable_6*
update_slots(*
_output_shapes

:
�
(Adagrad_2/update_Variable_7/ApplyAdagradApplyAdagrad
Variable_7Variable_7/AdagradAdagrad_2/learning_rategradients_3/AddN*
update_slots(*
_output_shapes

:*
use_locking( *
T0*
_class
loc:@Variable_7
g
	Adagrad_2NoOp)^Adagrad_2/update_Variable_6/ApplyAdagrad)^Adagrad_2/update_Variable_7/ApplyAdagrad
�
init_3NoOp^Variable/Assign^Variable_1/Assign^Variable_2/Adagrad/Assign^Variable_2/Assign^Variable_3/Adagrad/Assign^Variable_3/Assign^Variable_4/Adagrad/Assign^Variable_4/Assign^Variable_5/Adagrad/Assign^Variable_5/Assign^Variable_6/Adagrad/Assign^Variable_6/Assign^Variable_7/Adagrad/Assign^Variable_7/Assign
T
ArgMax_3/dimensionConst*
value	B :*
dtype0*
_output_shapes
: 
�
ArgMax_3ArgMaxNormal_2/prob/ExpArgMax_3/dimension*
T0*
output_type0	*#
_output_shapes
:���������*

Tidx0
P

save/ConstConst*
_output_shapes
: *
valueB Bmodel*
dtype0
�
save/StringJoin/inputs_1Const*<
value3B1 B+_temp_012e74441a5843f780e7d04488a5fe77/part*
dtype0*
_output_shapes
: 
u
save/StringJoin
StringJoin
save/Constsave/StringJoin/inputs_1*
	separator *
N*
_output_shapes
: 
Q
save/num_shardsConst*
_output_shapes
: *
value	B :*
dtype0
\
save/ShardedFilename/shardConst*
value	B : *
dtype0*
_output_shapes
: 
}
save/ShardedFilenameShardedFilenamesave/StringJoinsave/ShardedFilename/shardsave/num_shards*
_output_shapes
: 
�
save/SaveV2/tensor_namesConst*�
value�B�BVariableB
Variable_1B
Variable_2BVariable_2/AdagradB
Variable_3BVariable_3/AdagradB
Variable_4BVariable_4/AdagradB
Variable_5BVariable_5/AdagradB
Variable_6BVariable_6/AdagradB
Variable_7BVariable_7/Adagrad*
dtype0*
_output_shapes
:

save/SaveV2/shape_and_slicesConst*
dtype0*
_output_shapes
:*/
value&B$B B B B B B B B B B B B B B 
�
save/SaveV2SaveV2save/ShardedFilenamesave/SaveV2/tensor_namessave/SaveV2/shape_and_slicesVariable
Variable_1
Variable_2Variable_2/Adagrad
Variable_3Variable_3/Adagrad
Variable_4Variable_4/Adagrad
Variable_5Variable_5/Adagrad
Variable_6Variable_6/Adagrad
Variable_7Variable_7/Adagrad*
dtypes
2
�
save/control_dependencyIdentitysave/ShardedFilename^save/SaveV2*
T0*'
_class
loc:@save/ShardedFilename*
_output_shapes
: 
�
+save/MergeV2Checkpoints/checkpoint_prefixesPacksave/ShardedFilename^save/control_dependency*
T0*

axis *
N*
_output_shapes
:
}
save/MergeV2CheckpointsMergeV2Checkpoints+save/MergeV2Checkpoints/checkpoint_prefixes
save/Const*
delete_old_dirs(
z
save/IdentityIdentity
save/Const^save/MergeV2Checkpoints^save/control_dependency*
T0*
_output_shapes
: 
�
save/RestoreV2/tensor_namesConst*�
value�B�BVariableB
Variable_1B
Variable_2BVariable_2/AdagradB
Variable_3BVariable_3/AdagradB
Variable_4BVariable_4/AdagradB
Variable_5BVariable_5/AdagradB
Variable_6BVariable_6/AdagradB
Variable_7BVariable_7/Adagrad*
dtype0*
_output_shapes
:
�
save/RestoreV2/shape_and_slicesConst*/
value&B$B B B B B B B B B B B B B B *
dtype0*
_output_shapes
:
�
save/RestoreV2	RestoreV2
save/Constsave/RestoreV2/tensor_namessave/RestoreV2/shape_and_slices*L
_output_shapes:
8::::::::::::::*
dtypes
2
�
save/AssignAssignVariablesave/RestoreV2*
T0*
_class
loc:@Variable*
validate_shape(*
_output_shapes

:*
use_locking(
�
save/Assign_1Assign
Variable_1save/RestoreV2:1*
validate_shape(*
_output_shapes
:*
use_locking(*
T0*
_class
loc:@Variable_1
�
save/Assign_2Assign
Variable_2save/RestoreV2:2*
use_locking(*
T0*
_class
loc:@Variable_2*
validate_shape(*
_output_shapes

:
�
save/Assign_3AssignVariable_2/Adagradsave/RestoreV2:3*
use_locking(*
T0*
_class
loc:@Variable_2*
validate_shape(*
_output_shapes

:
�
save/Assign_4Assign
Variable_3save/RestoreV2:4*
T0*
_class
loc:@Variable_3*
validate_shape(*
_output_shapes

:*
use_locking(
�
save/Assign_5AssignVariable_3/Adagradsave/RestoreV2:5*
_output_shapes

:*
use_locking(*
T0*
_class
loc:@Variable_3*
validate_shape(
�
save/Assign_6Assign
Variable_4save/RestoreV2:6*
_class
loc:@Variable_4*
validate_shape(*
_output_shapes

:*
use_locking(*
T0
�
save/Assign_7AssignVariable_4/Adagradsave/RestoreV2:7*
_output_shapes

:*
use_locking(*
T0*
_class
loc:@Variable_4*
validate_shape(
�
save/Assign_8Assign
Variable_5save/RestoreV2:8*
T0*
_class
loc:@Variable_5*
validate_shape(*
_output_shapes

:*
use_locking(
�
save/Assign_9AssignVariable_5/Adagradsave/RestoreV2:9*
_output_shapes

:*
use_locking(*
T0*
_class
loc:@Variable_5*
validate_shape(
�
save/Assign_10Assign
Variable_6save/RestoreV2:10*
validate_shape(*
_output_shapes

:*
use_locking(*
T0*
_class
loc:@Variable_6
�
save/Assign_11AssignVariable_6/Adagradsave/RestoreV2:11*
use_locking(*
T0*
_class
loc:@Variable_6*
validate_shape(*
_output_shapes

:
�
save/Assign_12Assign
Variable_7save/RestoreV2:12*
use_locking(*
T0*
_class
loc:@Variable_7*
validate_shape(*
_output_shapes

:
�
save/Assign_13AssignVariable_7/Adagradsave/RestoreV2:13*
validate_shape(*
_output_shapes

:*
use_locking(*
T0*
_class
loc:@Variable_7
�
save/restore_shardNoOp^save/Assign^save/Assign_1^save/Assign_10^save/Assign_11^save/Assign_12^save/Assign_13^save/Assign_2^save/Assign_3^save/Assign_4^save/Assign_5^save/Assign_6^save/Assign_7^save/Assign_8^save/Assign_9
-
save/restore_allNoOp^save/restore_shard "<
save/Const:0save/Identity:0save/restore_all (5 @F8"�
trainable_variables��
9

Variable:0Variable/AssignVariable/read:02zeros:08
A
Variable_1:0Variable_1/AssignVariable_1/read:02	zeros_1:08
>
Variable_2:0Variable_2/AssignVariable_2/read:02ones:08
@
Variable_3:0Variable_3/AssignVariable_3/read:02ones_1:08
@
Variable_4:0Variable_4/AssignVariable_4/read:02ones_2:08
@
Variable_5:0Variable_5/AssignVariable_5/read:02ones_3:08
@
Variable_6:0Variable_6/AssignVariable_6/read:02ones_4:08
@
Variable_7:0Variable_7/AssignVariable_7/read:02ones_5:08">
train_op2
0
GradientDescent
Adagrad
	Adagrad_1
	Adagrad_2"�	
	variables�	�	
9

Variable:0Variable/AssignVariable/read:02zeros:08
A
Variable_1:0Variable_1/AssignVariable_1/read:02	zeros_1:08
>
Variable_2:0Variable_2/AssignVariable_2/read:02ones:08
@
Variable_3:0Variable_3/AssignVariable_3/read:02ones_1:08
t
Variable_2/Adagrad:0Variable_2/Adagrad/AssignVariable_2/Adagrad/read:02&Variable_2/Adagrad/Initializer/Const:0
t
Variable_3/Adagrad:0Variable_3/Adagrad/AssignVariable_3/Adagrad/read:02&Variable_3/Adagrad/Initializer/Const:0
@
Variable_4:0Variable_4/AssignVariable_4/read:02ones_2:08
@
Variable_5:0Variable_5/AssignVariable_5/read:02ones_3:08
t
Variable_4/Adagrad:0Variable_4/Adagrad/AssignVariable_4/Adagrad/read:02&Variable_4/Adagrad/Initializer/Const:0
t
Variable_5/Adagrad:0Variable_5/Adagrad/AssignVariable_5/Adagrad/read:02&Variable_5/Adagrad/Initializer/Const:0
@
Variable_6:0Variable_6/AssignVariable_6/read:02ones_4:08
@
Variable_7:0Variable_7/AssignVariable_7/read:02ones_5:08
t
Variable_6/Adagrad:0Variable_6/Adagrad/AssignVariable_6/Adagrad/read:02&Variable_6/Adagrad/Initializer/Const:0
t
Variable_7/Adagrad:0Variable_7/Adagrad/AssignVariable_7/Adagrad/read:02&Variable_7/Adagrad/Initializer/Const:0*�
serving_defaultv
+
x&
Placeholder_6:0���������+

prediction

ArgMax_3:0	���������tensorflow/serving/predict