# -*- Octave -*-

#WAVWRI Saves Microsoft Windows .WAV format sound files.
#   WAVWRI(y,Fs,res,nchannel,wavefile,user_limit) saves a .WAV format file.
#
#   The input arguments for WAVWRITE are as follows:
#
#       waveData    The sampled data to save
#       sRate       The rate at which the data was sampled
#       res         Resulution, 8 or 16 (bits/sample)
#       nchannel    Number of channels, 1 or 2
#       wavefile    A string containing the name of the .WAV file to create
#       user_limit  OPTIONAL: if supplied, the data will be treated as if it had
#                   maximum magnitude of user_limit.  Otherwise the data will be
#                   scaled so that its actual maximum magnitude goes to full scale.
#                   So, if you want the actual values of your data to appear in the
#                   WAV file, despite the fact that the max. mag. of your data may
#                   not be full scale, for 16-bit you would supply a user_limit of
#                   32767.

function savewav(waveData,sRate,res,nchannel,wavefile,user_limit)

  if (nargin != 5 && nargin != 6)
    error(['WAVWRI needs 5 or 6 arguments, got ' int2str(nargin)]);
  endif

  limit = max(max(abs(waveData)));

  if (nargin == 6)
    if (limit <= user_limit)
      limit = user_limit;
    else
      error(['a limit of ' num2str(user_limit) ' was passed in but actual limit is ' num2str(limit)]);
    endif
  endif

  waveData = waveData/limit*(2^(res-1) - 1);

  if (res == 8)
    waveData = waveData+128; 
  endif

  [fid, msg] = fopen(wavefile,"wb","ieee-le");

  if(ferror(fid))
    error(["Can't open .WAV file " wavefile " for output!"]);
  else
    [m,n]=size(waveData);
    nsamples=m*n;
    fac=res/8;
    
    riffsize=36+nsamples*fac;
    
    # write riff chunk
    fputs(fid,'RIFF');
    fwrite(fid,riffsize,'ulong');
    fputs(fid,'WAVE');
    
    # write format sub-chunk
    fputs(fid,'fmt ');
    fwrite(fid,16,'ulong');
    
    fwrite(fid,1,'ushort');         # PCM format
    fwrite(fid,nchannel,'ushort');  # 1 or 2 channel
    fwrite(fid,sRate,'ulong');      # samples per second
    fwrite(fid,fac*sRate*nchannel,'ulong');  # average bytes per second
    fwrite(fid,fac*nchannel,'ushort');       # block alignment
    fwrite(fid,fac*8,'ushort');     # bits per sample
    
    # write data sub-chunck
    fputs(fid,'data');
    fwrite(fid,nsamples*fac,'ulong');
    
    if nchannel==2 
      waveData=waveData(:);  
    endif   # Wenn Stereo
    
    if (fac == 2)
      fwrite(fid,waveData,'ushort');   # 16 bit
    else
      fwrite(fid,waveData,'uchar');    #  8 bit
    endif

    fclose(fid);
  endif
endfunction

