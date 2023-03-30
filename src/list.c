#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#define nullptr NULL
#define DEBUG 0

typedef struct Node
{
    void *data;
    struct Node *next;
} Node;

int List_len(Node *l) 
{
    int len = 0;
    Node *temp = l;

    while (temp != nullptr) {
        len++;
        temp = temp->next;
    }
    return len;
}

void *List_at(Node *l, int index)
{
    int len = List_len(l);
    assert((index >= 0) && (index < len));

    Node *temp = l;
    for (int i = 0; i < index; i++) temp = temp->next;
    return temp->data;
}

Node *List_insert(Node *head, int index, void *v) // TODO: deal with the returned list in codegen; do not make this transparent to the caller
{
    int len = List_len(head); assert((index >= 0) && (index <= len));
    
    Node *curr = head;
    Node *prev = nullptr;

    for (int i = 0; i < index; i++) {
        prev = curr;
        curr = curr->next;
    }

    Node *node = malloc(sizeof(*node)); assert(node);

    if (prev == nullptr) { // adding to the head of the list
        node->data = v;
        node->next = curr;
        return node;
    } else {
        prev->next = node;
        node->next = curr;
        node->data = v;
        return head;
    }

    return head; // suppress warnings
}

// for debugging -- TODO: add list_to_string in builtins?
void List_print(Node *l)
{
    Node *t = l;
    int i = 0;
    int len = List_len(l);
    while (i < len) {
        int *data = (int *)List_at(l, i); 
        printf("%d\n", *data);
        t = t->next;
        i = i + 1;
    }
}

#if DEBUG
int main()
{
    int *a = malloc(sizeof(a));
    int *b = malloc(sizeof(b));
    int *c = malloc(sizeof(c));
    
    *a = 1; *b = 2; *c = 3;

    Node *l;
    
    l = List_insert(l, 0, a);
    l = List_insert(l, 1, b); 
    l = List_insert(l, 2, c);

    printf("%d\n", List_len(l));
    // printf("%d\n", *(int *)(List_at(l, 0)));

    List_print(l);
}
#endif


// ; ModuleID = 'list.c'
// source_filename = "list.c"
// target datalayout = "e-m:o-i64:64-i128:128-n32:64-S128"
// target triple = "arm64-apple-macosx13.0.0"

// %struct.Node = type { i8*, %struct.Node* }

// @__func__.List_at = private unnamed_addr constant [8 x i8] c"List_at\00", align 1
// @.str = private unnamed_addr constant [7 x i8] c"list.c\00", align 1
// @.str.1 = private unnamed_addr constant [30 x i8] c"(index >= 0) && (index < len)\00", align 1
// @__func__.List_insert = private unnamed_addr constant [12 x i8] c"List_insert\00", align 1
// @.str.2 = private unnamed_addr constant [31 x i8] c"(index >= 0) && (index <= len)\00", align 1
// @.str.3 = private unnamed_addr constant [5 x i8] c"node\00", align 1
// @.str.4 = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1

// ; Function Attrs: noinline nounwind optnone ssp uwtable
// define i32 @List_len(%struct.Node* %0) #0 {
//   %2 = alloca %struct.Node*, align 8
//   %3 = alloca i32, align 4
//   %4 = alloca %struct.Node*, align 8
//   store %struct.Node* %0, %struct.Node** %2, align 8
//   store i32 0, i32* %3, align 4
//   %5 = load %struct.Node*, %struct.Node** %2, align 8
//   store %struct.Node* %5, %struct.Node** %4, align 8
//   br label %6

// 6:                                                ; preds = %9, %1
//   %7 = load %struct.Node*, %struct.Node** %4, align 8
//   %8 = icmp ne %struct.Node* %7, null
//   br i1 %8, label %9, label %15

// 9:                                                ; preds = %6
//   %10 = load i32, i32* %3, align 4
//   %11 = add nsw i32 %10, 1
//   store i32 %11, i32* %3, align 4
//   %12 = load %struct.Node*, %struct.Node** %4, align 8
//   %13 = getelementptr inbounds %struct.Node, %struct.Node* %12, i32 0, i32 1
//   %14 = load %struct.Node*, %struct.Node** %13, align 8
//   store %struct.Node* %14, %struct.Node** %4, align 8
//   br label %6, !llvm.loop !10

// 15:                                               ; preds = %6
//   %16 = load i32, i32* %3, align 4
//   ret i32 %16
// }

// ; Function Attrs: noinline nounwind optnone ssp uwtable
// define i8* @List_at(%struct.Node* %0, i32 %1) #0 {
//   %3 = alloca %struct.Node*, align 8
//   %4 = alloca i32, align 4
//   %5 = alloca i32, align 4
//   %6 = alloca %struct.Node*, align 8
//   %7 = alloca i32, align 4
//   store %struct.Node* %0, %struct.Node** %3, align 8
//   store i32 %1, i32* %4, align 4
//   %8 = load %struct.Node*, %struct.Node** %3, align 8
//   %9 = call i32 @List_len(%struct.Node* %8)
//   store i32 %9, i32* %5, align 4
//   %10 = load i32, i32* %4, align 4
//   %11 = icmp sge i32 %10, 0
//   br i1 %11, label %12, label %16

// 12:                                               ; preds = %2
//   %13 = load i32, i32* %4, align 4
//   %14 = load i32, i32* %5, align 4
//   %15 = icmp slt i32 %13, %14
//   br label %16

// 16:                                               ; preds = %12, %2
//   %17 = phi i1 [ false, %2 ], [ %15, %12 ]
//   %18 = xor i1 %17, true
//   %19 = zext i1 %18 to i32
//   %20 = sext i32 %19 to i64
//   %21 = icmp ne i64 %20, 0
//   br i1 %21, label %22, label %24

// 22:                                               ; preds = %16
//   call void @__assert_rtn(i8* getelementptr inbounds ([8 x i8], [8 x i8]* @__func__.List_at, i64 0, i64 0), i8* getelementptr inbounds ([7 x i8], [7 x i8]* @.str, i64 0, i64 0), i32 28, i8* getelementptr inbounds ([30 x i8], [30 x i8]* @.str.1, i64 0, i64 0)) #4
//   unreachable

// 23:                                               ; No predecessors!
//   br label %25

// 24:                                               ; preds = %16
//   br label %25

// 25:                                               ; preds = %24, %23
//   %26 = load %struct.Node*, %struct.Node** %3, align 8
//   store %struct.Node* %26, %struct.Node** %6, align 8
//   store i32 0, i32* %7, align 4
//   br label %27

// 27:                                               ; preds = %35, %25
//   %28 = load i32, i32* %7, align 4
//   %29 = load i32, i32* %4, align 4
//   %30 = icmp slt i32 %28, %29
//   br i1 %30, label %31, label %38

// 31:                                               ; preds = %27
//   %32 = load %struct.Node*, %struct.Node** %6, align 8
//   %33 = getelementptr inbounds %struct.Node, %struct.Node* %32, i32 0, i32 1
//   %34 = load %struct.Node*, %struct.Node** %33, align 8
//   store %struct.Node* %34, %struct.Node** %6, align 8
//   br label %35

// 35:                                               ; preds = %31
//   %36 = load i32, i32* %7, align 4
//   %37 = add nsw i32 %36, 1
//   store i32 %37, i32* %7, align 4
//   br label %27, !llvm.loop !12

// 38:                                               ; preds = %27
//   %39 = load %struct.Node*, %struct.Node** %6, align 8
//   %40 = getelementptr inbounds %struct.Node, %struct.Node* %39, i32 0, i32 0
//   %41 = load i8*, i8** %40, align 8
//   ret i8* %41
// }

// ; Function Attrs: cold noreturn
// declare void @__assert_rtn(i8*, i8*, i32, i8*) #1

// ; Function Attrs: noinline nounwind optnone ssp uwtable
// define %struct.Node* @List_insert(%struct.Node* %0, i32 %1, i8* %2) #0 {
//   %4 = alloca %struct.Node*, align 8
//   %5 = alloca %struct.Node*, align 8
//   %6 = alloca i32, align 4
//   %7 = alloca i8*, align 8
//   %8 = alloca i32, align 4
//   %9 = alloca %struct.Node*, align 8
//   %10 = alloca %struct.Node*, align 8
//   %11 = alloca i32, align 4
//   %12 = alloca %struct.Node*, align 8
//   store %struct.Node* %0, %struct.Node** %5, align 8
//   store i32 %1, i32* %6, align 4
//   store i8* %2, i8** %7, align 8
//   %13 = load %struct.Node*, %struct.Node** %5, align 8
//   %14 = call i32 @List_len(%struct.Node* %13)
//   store i32 %14, i32* %8, align 4
//   %15 = load i32, i32* %6, align 4
//   %16 = icmp sge i32 %15, 0
//   br i1 %16, label %17, label %21

// 17:                                               ; preds = %3
//   %18 = load i32, i32* %6, align 4
//   %19 = load i32, i32* %8, align 4
//   %20 = icmp sle i32 %18, %19
//   br label %21

// 21:                                               ; preds = %17, %3
//   %22 = phi i1 [ false, %3 ], [ %20, %17 ]
//   %23 = xor i1 %22, true
//   %24 = zext i1 %23 to i32
//   %25 = sext i32 %24 to i64
//   %26 = icmp ne i64 %25, 0
//   br i1 %26, label %27, label %29

// 27:                                               ; preds = %21
//   call void @__assert_rtn(i8* getelementptr inbounds ([12 x i8], [12 x i8]* @__func__.List_insert, i64 0, i64 0), i8* getelementptr inbounds ([7 x i8], [7 x i8]* @.str, i64 0, i64 0), i32 37, i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.2, i64 0, i64 0)) #4
//   unreachable

// 28:                                               ; No predecessors!
//   br label %30

// 29:                                               ; preds = %21
//   br label %30

// 30:                                               ; preds = %29, %28
//   %31 = load %struct.Node*, %struct.Node** %5, align 8
//   store %struct.Node* %31, %struct.Node** %9, align 8
//   store %struct.Node* null, %struct.Node** %10, align 8
//   store i32 0, i32* %11, align 4
//   br label %32

// 32:                                               ; preds = %41, %30
//   %33 = load i32, i32* %11, align 4
//   %34 = load i32, i32* %6, align 4
//   %35 = icmp slt i32 %33, %34
//   br i1 %35, label %36, label %44

// 36:                                               ; preds = %32
//   %37 = load %struct.Node*, %struct.Node** %9, align 8
//   store %struct.Node* %37, %struct.Node** %10, align 8
//   %38 = load %struct.Node*, %struct.Node** %9, align 8
//   %39 = getelementptr inbounds %struct.Node, %struct.Node* %38, i32 0, i32 1
//   %40 = load %struct.Node*, %struct.Node** %39, align 8
//   store %struct.Node* %40, %struct.Node** %9, align 8
//   br label %41

// 41:                                               ; preds = %36
//   %42 = load i32, i32* %11, align 4
//   %43 = add nsw i32 %42, 1
//   store i32 %43, i32* %11, align 4
//   br label %32, !llvm.loop !13

// 44:                                               ; preds = %32
//   %45 = call i8* @malloc(i64 16) #5
//   %46 = bitcast i8* %45 to %struct.Node*
//   store %struct.Node* %46, %struct.Node** %12, align 8
//   %47 = load %struct.Node*, %struct.Node** %12, align 8
//   %48 = icmp ne %struct.Node* %47, null
//   %49 = xor i1 %48, true
//   %50 = zext i1 %49 to i32
//   %51 = sext i32 %50 to i64
//   %52 = icmp ne i64 %51, 0
//   br i1 %52, label %53, label %55

// 53:                                               ; preds = %44
//   call void @__assert_rtn(i8* getelementptr inbounds ([12 x i8], [12 x i8]* @__func__.List_insert, i64 0, i64 0), i8* getelementptr inbounds ([7 x i8], [7 x i8]* @.str, i64 0, i64 0), i32 47, i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.3, i64 0, i64 0)) #4
//   unreachable

// 54:                                               ; No predecessors!
//   br label %56

// 55:                                               ; preds = %44
//   br label %56

// 56:                                               ; preds = %55, %54
//   %57 = load %struct.Node*, %struct.Node** %10, align 8
//   %58 = icmp eq %struct.Node* %57, null
//   br i1 %58, label %59, label %67

// 59:                                               ; preds = %56
//   %60 = load i8*, i8** %7, align 8
//   %61 = load %struct.Node*, %struct.Node** %12, align 8
//   %62 = getelementptr inbounds %struct.Node, %struct.Node* %61, i32 0, i32 0
//   store i8* %60, i8** %62, align 8
//   %63 = load %struct.Node*, %struct.Node** %9, align 8
//   %64 = load %struct.Node*, %struct.Node** %12, align 8
//   %65 = getelementptr inbounds %struct.Node, %struct.Node* %64, i32 0, i32 1
//   store %struct.Node* %63, %struct.Node** %65, align 8
//   %66 = load %struct.Node*, %struct.Node** %12, align 8
//   store %struct.Node* %66, %struct.Node** %4, align 8
//   br label %78

// 67:                                               ; preds = %56
//   %68 = load %struct.Node*, %struct.Node** %12, align 8
//   %69 = load %struct.Node*, %struct.Node** %10, align 8
//   %70 = getelementptr inbounds %struct.Node, %struct.Node* %69, i32 0, i32 1
//   store %struct.Node* %68, %struct.Node** %70, align 8
//   %71 = load %struct.Node*, %struct.Node** %9, align 8
//   %72 = load %struct.Node*, %struct.Node** %12, align 8
//   %73 = getelementptr inbounds %struct.Node, %struct.Node* %72, i32 0, i32 1
//   store %struct.Node* %71, %struct.Node** %73, align 8
//   %74 = load i8*, i8** %7, align 8
//   %75 = load %struct.Node*, %struct.Node** %12, align 8
//   %76 = getelementptr inbounds %struct.Node, %struct.Node* %75, i32 0, i32 0
//   store i8* %74, i8** %76, align 8
//   %77 = load %struct.Node*, %struct.Node** %5, align 8
//   store %struct.Node* %77, %struct.Node** %4, align 8
//   br label %78

// 78:                                               ; preds = %67, %59
//   %79 = load %struct.Node*, %struct.Node** %4, align 8
//   ret %struct.Node* %79
// }

// ; Function Attrs: allocsize(0)
// declare i8* @malloc(i64) #2

// ; Function Attrs: noinline nounwind optnone ssp uwtable
// define void @List_print(%struct.Node* %0) #0 {
//   %2 = alloca %struct.Node*, align 8
//   %3 = alloca %struct.Node*, align 8
//   %4 = alloca i32, align 4
//   %5 = alloca i32, align 4
//   %6 = alloca i32*, align 8
//   store %struct.Node* %0, %struct.Node** %2, align 8
//   %7 = load %struct.Node*, %struct.Node** %2, align 8
//   store %struct.Node* %7, %struct.Node** %3, align 8
//   store i32 0, i32* %4, align 4
//   %8 = load %struct.Node*, %struct.Node** %2, align 8
//   %9 = call i32 @List_len(%struct.Node* %8)
//   store i32 %9, i32* %5, align 4
//   br label %10

// 10:                                               ; preds = %14, %1
//   %11 = load i32, i32* %4, align 4
//   %12 = load i32, i32* %5, align 4
//   %13 = icmp slt i32 %11, %12
//   br i1 %13, label %14, label %27

// 14:                                               ; preds = %10
//   %15 = load %struct.Node*, %struct.Node** %2, align 8
//   %16 = load i32, i32* %4, align 4
//   %17 = call i8* @List_at(%struct.Node* %15, i32 %16)
//   %18 = bitcast i8* %17 to i32*
//   store i32* %18, i32** %6, align 8
//   %19 = load i32*, i32** %6, align 8
//   %20 = load i32, i32* %19, align 4
//   %21 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.4, i64 0, i64 0), i32 %20)
//   %22 = load %struct.Node*, %struct.Node** %3, align 8
//   %23 = getelementptr inbounds %struct.Node, %struct.Node* %22, i32 0, i32 1
//   %24 = load %struct.Node*, %struct.Node** %23, align 8
//   store %struct.Node* %24, %struct.Node** %3, align 8
//   %25 = load i32, i32* %4, align 4
//   %26 = add nsw i32 %25, 1
//   store i32 %26, i32* %4, align 4
//   br label %10, !llvm.loop !14

// 27:                                               ; preds = %10
//   ret void
// }

// declare i32 @printf(i8*, ...) #3

// ; Function Attrs: noinline nounwind optnone ssp uwtable
// define i32 @main() #0 {
//   %1 = alloca i32*, align 8
//   %2 = alloca i32*, align 8
//   %3 = alloca i32*, align 8
//   %4 = alloca %struct.Node*, align 8
//   %5 = call i8* @malloc(i64 8) #5
//   %6 = bitcast i8* %5 to i32*
//   store i32* %6, i32** %1, align 8
//   %7 = call i8* @malloc(i64 8) #5
//   %8 = bitcast i8* %7 to i32*
//   store i32* %8, i32** %2, align 8
//   %9 = call i8* @malloc(i64 8) #5
//   %10 = bitcast i8* %9 to i32*
//   store i32* %10, i32** %3, align 8
//   %11 = load i32*, i32** %1, align 8
//   store i32 1, i32* %11, align 4
//   %12 = load i32*, i32** %2, align 8
//   store i32 2, i32* %12, align 4
//   %13 = load i32*, i32** %3, align 8
//   store i32 3, i32* %13, align 4
//   %14 = load %struct.Node*, %struct.Node** %4, align 8
//   %15 = load i32*, i32** %1, align 8
//   %16 = bitcast i32* %15 to i8*
//   %17 = call %struct.Node* @List_insert(%struct.Node* %14, i32 0, i8* %16)
//   store %struct.Node* %17, %struct.Node** %4, align 8
//   %18 = load %struct.Node*, %struct.Node** %4, align 8
//   %19 = load i32*, i32** %2, align 8
//   %20 = bitcast i32* %19 to i8*
//   %21 = call %struct.Node* @List_insert(%struct.Node* %18, i32 1, i8* %20)
//   store %struct.Node* %21, %struct.Node** %4, align 8
//   %22 = load %struct.Node*, %struct.Node** %4, align 8
//   %23 = load i32*, i32** %3, align 8
//   %24 = bitcast i32* %23 to i8*
//   %25 = call %struct.Node* @List_insert(%struct.Node* %22, i32 2, i8* %24)
//   store %struct.Node* %25, %struct.Node** %4, align 8
//   %26 = load %struct.Node*, %struct.Node** %4, align 8
//   %27 = call i32 @List_len(%struct.Node* %26)
//   %28 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.4, i64 0, i64 0), i32 %27)
//   %29 = load %struct.Node*, %struct.Node** %4, align 8
//   call void @List_print(%struct.Node* %29)
//   ret i32 0
// }

// attributes #0 = { noinline nounwind optnone ssp uwtable "frame-pointer"="non-leaf" "min-legal-vector-width"="0" "no-trapping-math"="true" "probe-stack"="__chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+crc,+crypto,+dotprod,+fp-armv8,+fp16fml,+fullfp16,+lse,+neon,+ras,+rcpc,+rdm,+sha2,+sha3,+sm4,+v8.5a,+zcm,+zcz" }
// attributes #1 = { cold noreturn "disable-tail-calls"="true" "frame-pointer"="non-leaf" "no-trapping-math"="true" "probe-stack"="__chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+crc,+crypto,+dotprod,+fp-armv8,+fp16fml,+fullfp16,+lse,+neon,+ras,+rcpc,+rdm,+sha2,+sha3,+sm4,+v8.5a,+zcm,+zcz" }
// attributes #2 = { allocsize(0) "frame-pointer"="non-leaf" "no-trapping-math"="true" "probe-stack"="__chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+crc,+crypto,+dotprod,+fp-armv8,+fp16fml,+fullfp16,+lse,+neon,+ras,+rcpc,+rdm,+sha2,+sha3,+sm4,+v8.5a,+zcm,+zcz" }
// attributes #3 = { "frame-pointer"="non-leaf" "no-trapping-math"="true" "probe-stack"="__chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+crc,+crypto,+dotprod,+fp-armv8,+fp16fml,+fullfp16,+lse,+neon,+ras,+rcpc,+rdm,+sha2,+sha3,+sm4,+v8.5a,+zcm,+zcz" }
// attributes #4 = { cold noreturn }
// attributes #5 = { allocsize(0) }

// !llvm.module.flags = !{!0, !1, !2, !3, !4, !5, !6, !7, !8}
// !llvm.ident = !{!9}

// !0 = !{i32 2, !"SDK Version", [2 x i32] [i32 13, i32 1]}
// !1 = !{i32 1, !"wchar_size", i32 4}
// !2 = !{i32 1, !"branch-target-enforcement", i32 0}
// !3 = !{i32 1, !"sign-return-address", i32 0}
// !4 = !{i32 1, !"sign-return-address-all", i32 0}
// !5 = !{i32 1, !"sign-return-address-with-bkey", i32 0}
// !6 = !{i32 7, !"PIC Level", i32 2}
// !7 = !{i32 7, !"uwtable", i32 1}
// !8 = !{i32 7, !"frame-pointer", i32 1}
// !9 = !{!"Apple clang version 14.0.0 (clang-1400.0.29.202)"}
// !10 = distinct !{!10, !11}
// !11 = !{!"llvm.loop.mustprogress"}
// !12 = distinct !{!12, !11}
// !13 = distinct !{!13, !11}
// !14 = distinct !{!14, !11}
