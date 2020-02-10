C lpcio_util.inc
C usefull for compilers missing certain bit manipulation
C routines
C M.A.Delsuc, 14 june 1993
C
      function ishft(m,k)
      integer m,k,ishft
      if (k.gt.0) then
           ishft = lshift(m,k)
      else
           ishft = rshift(m,abs(k))
      endif
      return
      end

      function ior(m,n)
      integer ior,m,n
      ior = or(m,n)
      return
      end

      function ibits(m,k,l)
      integer ibits,m,k,l,mask
      mask = -1
      mask = lshift(mask,32-l-k-1)
      mask = rshift(mask,32-k)
      ibits = and(m,mask)
      return
      end
