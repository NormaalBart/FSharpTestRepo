module Types

type Symbol =
    | Cross
    | Nought

type FieldIndex = 
    | First
    | Second
    | Third

type FieldPosition = {
    RowIndex: FieldIndex;
    ColIndex: FieldIndex;
}

type Field =
    | Empty
    | Taken of Symbol

type Row = Field * Field * Field
type Board = Row * Row * Row
