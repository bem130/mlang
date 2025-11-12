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

  (func $__string_char_at (param $header_ptr i32) (param $index i32) (result i32)
    (local $data_ptr i32) (local $len i32) (local $char_value i32)
    (local $char_data_ptr i32) (local $result_header i32)
    local.get $header_ptr
    i32.load offset=4
    local.set $len
    local.get $index
    i32.const 0
    i32.lt_s
    local.get $index
    local.get $len
    i32.ge_s
    i32.or
    if
      unreachable
    end
    local.get $header_ptr
    i32.load offset=0
    local.set $data_ptr
    local.get $data_ptr
    local.get $index
    i32.add
    i32.load8_u
    local.set $char_value
    i32.const 1
    call $__alloc
    local.set $char_data_ptr
    local.get $char_data_ptr
    local.get $char_value
    i32.store8
    i32.const 12
    call $__alloc
    local.set $result_header
    local.get $result_header
    local.get $char_data_ptr
    i32.store offset=0
    local.get $result_header
    i32.const 1
    i32.store offset=4
    local.get $result_header
    i32.const 1
    i32.store offset=8
    local.get $result_header
  )

  (func $__vec_new_i32 (result i32)
    (local $data_ptr i32) (local $header_ptr i32)
    i32.const 16
    call $__alloc
    local.set $data_ptr
    i32.const 12
    call $__alloc
    local.set $header_ptr
    local.get $header_ptr
    local.get $data_ptr
    i32.store offset=0
    local.get $header_ptr
    i32.const 0
    i32.store offset=4
    local.get $header_ptr
    i32.const 4
    i32.store offset=8
    local.get $header_ptr
  )

  (func $__vec_push_i32 (param $vec i32) (param $value i32)
    (local $data_ptr i32) (local $len i32) (local $cap i32)
    (local $new_cap i32) (local $new_data_ptr i32)
    local.get $vec
    i32.load offset=0
    local.set $data_ptr
    local.get $vec
    i32.load offset=4
    local.set $len
    local.get $vec
    i32.load offset=8
    local.set $cap
    local.get $len
    local.get $cap
    i32.eq
    if
      local.get $cap
      i32.const 2
      i32.mul
      local.set $new_cap
      local.get $new_cap
      i32.const 0
      i32.eq
      if
        i32.const 4
        local.set $new_cap
      end
      local.get $new_cap
      i32.const 4
      i32.mul
      call $__alloc
      local.set $new_data_ptr
      local.get $new_data_ptr
      local.get $data_ptr
      local.get $len
      i32.const 4
      i32.mul
      memory.copy
      local.get $vec
      local.get $new_data_ptr
      i32.store offset=0
      local.get $vec
      local.get $new_cap
      i32.store offset=8
      local.get $new_data_ptr
      local.set $data_ptr
    end
    local.get $data_ptr
    local.get $len
    i32.const 4
    i32.mul
    i32.add
    local.get $value
    i32.store
    local.get $vec
    local.get $len
    i32.const 1
    i32.add
    i32.store offset=4
  )

  (func $__vec_get_i32 (param $vec i32) (param $index i32) (result i32)
    (local $len i32) (local $data_ptr i32)
    local.get $vec
    i32.load offset=4
    local.set $len
    local.get $index
    i32.const 0
    i32.lt_s
    local.get $index
    local.get $len
    i32.ge_s
    i32.or
    if
      unreachable
    end
    local.get $vec
    i32.load offset=0
    local.set $data_ptr
    local.get $data_ptr
    local.get $index
    i32.const 4
    i32.mul
    i32.add
    i32.load
  )

  (func $__vec_len_i32 (param $vec i32) (result i32)
    local.get $vec
    i32.load offset=4
  )

  ;; Reads a line from stdin, returns a new string.
  ;; Note: This is a simplified implementation. It reads a chunk of data,
  ;; trims trailing newline characters, and returns it as a string.
  (func $__read_line (result i32)
    (local $buf_ptr i32)
    (local $buf_len i32)
    (local $iovec_ptr i32)
    (local $nread_ptr i32)
    (local $nread i32)
    (local $new_data_ptr i32)
    (local $new_header_ptr i32)
    (local $new_len i32)

    ;; 1. Allocate a temporary buffer for reading
    i32.const 256
    local.set $buf_len
    local.get $buf_len
    call $__alloc
    local.set $buf_ptr

    ;; 2. Prepare iovec and nread pointers (using low memory addresses)
    i32.const 0
    local.set $iovec_ptr
    i32.const 8
    local.set $nread_ptr

    ;; Setup iovec[0]
    local.get $iovec_ptr
    local.get $buf_ptr
    i32.store offset=0 ;; iovec[0].buf
    local.get $iovec_ptr
    local.get $buf_len
    i32.store offset=4 ;; iovec[0].buf_len

    ;; 3. Call fd_read for stdin (fd=0)
    i32.const 0 ;; fd
    local.get $iovec_ptr
    i32.const 1 ;; iovecs_len
    local.get $nread_ptr
    call $fd_read
    drop ;; drop errno

    ;; 4. Get the number of bytes read
    local.get $nread_ptr
    i32.load
    local.set $nread

    ;; Trim trailing newline characters (\n or \r\n)
    ;; Loop structure:
    ;;  - If nread <= 0, break
    ;;  - Load last byte; if it's '\n' or '\r' decrement nread and continue loop
    ;;  - Otherwise break
    (block $trim_done
      (loop $trim_loop
        ;; if nread <= 0 then break
        local.get $nread
        i32.const 0
        i32.le_s
        br_if $trim_done

        ;; load last byte: buf_ptr + (nread - 1)
        local.get $buf_ptr
        local.get $nread
        i32.const 1
        i32.sub
        i32.add
        i32.load8_u
        local.set $new_len ;; use $new_len as a temporary holder for the byte

        ;; if byte == '\n' then decrement nread and continue
        local.get $new_len
        i32.const 10 ;; '\n'
        i32.eq
        if
          local.get $nread
          i32.const 1
          i32.sub
          local.set $nread
          br $trim_loop
        end

        ;; if byte == '\r' then decrement nread and continue
        local.get $new_len
        i32.const 13 ;; '\r'
        i32.eq
        if
          local.get $nread
          i32.const 1
          i32.sub
          local.set $nread
          br $trim_loop
        end

        ;; otherwise break out of trimming loop
        br $trim_done
      )
    )
    local.get $nread
    local.set $new_len

    ;; 5. Create the final string
    ;; Allocate memory for the string data
    local.get $new_len
    call $__alloc
    local.set $new_data_ptr

    ;; Copy data from the temp buffer
    local.get $new_data_ptr
    local.get $buf_ptr
    local.get $new_len
    memory.copy

    ;; Allocate memory for the string header
    i32.const 12
    call $__alloc
    local.set $new_header_ptr

    ;; Fill the header
    local.get $new_header_ptr
    local.get $new_data_ptr
    i32.store offset=0 ;; ptr
    local.get $new_header_ptr
    local.get $new_len
    i32.store offset=4 ;; len
    local.get $new_header_ptr
    local.get $new_len ;; cap = len
    i32.store offset=8 ;; cap

    ;; Return the header pointer
    local.get $new_header_ptr
  )