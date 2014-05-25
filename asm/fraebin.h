

/*
HEADER:     ;
TITLE:      Frankenstein Cross Assemblers;
VERSION:    2.0;
DESCRIPTION: "  Reconfigurable Cross-assembler producing Intel (TM)
        Hex format object records.  ";
FILENAME:   fraebin.h;
SEE-ALSO:   frapsub.c, fraosub.c;
AUTHORS:    Mark Zenier;
*/

/*
    description switch case actions for binary operators for
            both the parse and output phase expression
            evaluators
*/


        case IFC_MUL:
            etop *= (estkm1p--)->v;
            break;

        case IFC_DIV:
            if (etop != 0)
                etop = (estkm1p--)->v/etop;
            else
            {
                estkm1p--;
                FRAERR("division by zero attempted");
            }
            break;

        case IFC_ADD:
            etop += (estkm1p--)->v;
            break;

        case IFC_SUB:
            etop = (estkm1p--)->v - etop;
            break;

        case IFC_MOD:
            if (etop != 0)
                etop = (estkm1p--)->v % etop;
            else
            {
                estkm1p--;
                FRAERR("division by zero attempted");
            }
            break;

        case IFC_SHL:
            if (etop < 0) FRAERR("shift left by negative amount attempted");
            else          etop = etop < 32 ? estkm1p->v << etop : 0;
            estkm1p--;
            break;

        case IFC_SHRU:
            if (etop < 0) FRAERR("shift right by negative amount attempted");
            else          etop = etop < 32 ? (unsigned)estkm1p->v >> etop : 0;
            estkm1p--;
            break;

        case IFC_SHR:
            if (etop < 0) FRAERR("shift right by negative amount attempted");
            else          etop = (signed)estkm1p->v >> (etop < 32 ? etop : 31);
            estkm1p--;
            break;

        case IFC_AND:
            etop &= (estkm1p--)->v;
            break;

        case IFC_OR:
            etop |= (estkm1p--)->v;
            break;

        case IFC_XOR:
            etop ^= (estkm1p--)->v;
            break;

        case IFC_GT:
            etop = (estkm1p--)->v > etop ? 1 : 0;
            break;

        case IFC_GE:
            etop = (estkm1p--)->v >= etop ? 1 : 0;
            break;

        case IFC_LT:
            etop = (estkm1p--)->v < etop ? 1 : 0;
            break;

        case IFC_LE:
            etop = (estkm1p--)->v <= etop ? 1 : 0;
            break;

        case IFC_NE:
            etop = (estkm1p--)->v != etop ? 1 : 0;
            break;

        case IFC_EQ:
            etop = (estkm1p--)->v == etop ? 1 : 0;
            break;

