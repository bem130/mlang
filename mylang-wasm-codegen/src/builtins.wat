;; --- Built-in Helper Functions ---

  ;; Allocates memory by incrementing a heap pointer.
  ;; Does not implement freeing.
  (func $__alloc (param $size i32) (result i32)
    global.get $heap_ptr         ;; Return current heap pointer
    global.get $heap_ptr         ;; Push heap pointer again to calculate next
    local.get $size
    i32.add
    global.set $heap_ptr         ;; Update heap pointer
  )

  ;; Concatenates two strings, returns a new string.
  (func $__string_concat (param $s1_header_ptr i32) (param $s2_header_ptr i32) (result i32)
    (local $s1_data_ptr i32) (local $s1_len i32)
    (local $s2_data_ptr i32) (local $s2_len i32)
    (local $new_len i32) (local $new_cap i32)
    (local $new_data_ptr i32) (local $new_header_ptr i32)
    local.get $s1_header_ptr
    i32.load offset=0
    local.set $s1_data_ptr
    local.get $s1_header_ptr
    i32.load offset=4
    local.set $s1_len
    local.get $s2_header_ptr
    i32.load offset=0
    local.set $s2_data_ptr
    local.get $s2_header_ptr
    i32.load offset=4
    local.set $s2_len
    local.get $s1_len
    local.get $s2_len
    i32.add
    local.set $new_len
    local.get $new_len
    i32.const 2
    i32.mul
    local.set $new_cap
    local.get $new_cap
    call $__alloc
    local.set $new_data_ptr
    i32.const 12
    call $__alloc
    local.set $new_header_ptr
    local.get $new_data_ptr
    local.get $s1_data_ptr
    local.get $s1_len
    memory.copy
    local.get $new_data_ptr
    local.get $s1_len
    i32.add
    local.get $s2_data_ptr
    local.get $s2_len
    memory.copy
    local.get $new_header_ptr
    local.get $new_data_ptr
    i32.store offset=0
    local.get $new_header_ptr
    local.get $new_len
    i32.store offset=4
    local.get $new_header_ptr
    local.get $new_cap
    i32.store offset=8
    local.get $new_header_ptr
  )

  ;; Converts an i32 to a new string.
  (func $__i32_to_string (param $n i32) (result i32)
    (local $is_neg i32) (local $len i32) (local $temp_ptr i32) (local $write_ptr i32)
    (local $char_ptr i32) (local $cap i32) (local $header_ptr i32) (local $i i32)
    i32.const 16
    call $__alloc
    local.set $temp_ptr ;; Temp buffer for reversed digits
    local.get $temp_ptr
    local.set $write_ptr
    local.get $n
    i32.const 0
    i32.lt_s
    local.set $is_neg
    local.get $is_neg
    if
      local.get $n
      i32.const -1
      i32.mul
      local.set $n
    end
    local.get $n
    i32.const 0
    i32.eq
    if
      local.get $write_ptr
      i32.const 48 ;; '0'
      i32.store8
      local.get $write_ptr
      i32.const 1
      i32.add
      local.set $write_ptr
    else
      (loop $digits
        local.get $write_ptr
        local.get $n
        i32.const 10
        i32.rem_u
        i32.const 48
        i32.add
        i32.store8
        local.get $write_ptr
        i32.const 1
        i32.add
        local.set $write_ptr
        local.get $n
        i32.const 10
        i32.div_u
        local.set $n
        local.get $n
        i32.const 0
        i32.ne
        br_if $digits
      )
    end
    local.get $is_neg
    if
      local.get $write_ptr
      i32.const 45 ;; '-'
      i32.store8
      local.get $write_ptr
      i32.const 1
      i32.add
      local.set $write_ptr
    end
    local.get $write_ptr
    local.get $temp_ptr
    i32.sub
    local.set $len
    local.get $len
    local.set $cap
    local.get $cap
    call $__alloc
    local.set $char_ptr ;; Final buffer for correct-order string
    (loop $reverse
      local.get $char_ptr
      local.get $i
      i32.add
      local.get $write_ptr
      local.get $i
      i32.sub
      i32.const 1
      i32.sub
      i32.load8_u
      i32.store8
      local.get $i
      i32.const 1
      i32.add
      local.set $i
      local.get $i
      local.get $len
      i32.lt_s
      br_if $reverse
    )
    i32.const 12
    call $__alloc
    local.set $header_ptr
    local.get $header_ptr
    local.get $char_ptr
    i32.store offset=0
    local.get $header_ptr
    local.get $len
    i32.store offset=4
    local.get $header_ptr
    local.get $cap
    i32.store offset=8
    local.get $header_ptr
  )