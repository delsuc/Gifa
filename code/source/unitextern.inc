C Declaration of most of the internal functions used in Gifa
C 
C Certain compiler will complain about "declared EXTERNAL but never used."
C and will give you a warning
C DO NOT care.
C
      external itoh,itop,htoi,ptoi,itohr,itopr,
     *         htoir,ptoir,alphanum,isalphanum,isnumeral,
     *         checkdim,stoi,itos,itosr,stoir,
     *         itok,itokr,ktoir,ktoi,
     *         itot,itotr,ttoir,ttoi,
     *         d_itohr,d_htoir,d_itopr,d_ptoir
      real itoh,itop,itohr,itopr,htoir,ptoir,itos,itosr,stoir,
     *     itok,itokr,ktoir,itot,itotr,ttoir,
     *     d_itohr,d_htoir,d_itopr,d_ptoir
      integer htoi,ptoi,stoi,ktoi,ttoi
      logical alphanum,checkdim,isalphanum,isnumeral

