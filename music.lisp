(import (OpenAL version-1-1))
(define environment-sound-file "music/night2.ogg")


(define device (alcOpenDevice #false))
(define context (alcCreateContext device #false))
(alcMakeContextCurrent context)

(print "OpenAL version: " (alGetString AL_VERSION))
(print "OpenAL vendor: " (alGetString AL_VENDOR))
(print "OpenAL renderer: " (alGetString AL_RENDERER))

(import (lib vorbis))
(define vf (make-OggVorbis_File))

(print "opening sound file: " (ov_fopen (c-string environment-sound-file) vf))
   ;; 0 indicates success
   ;; less than zero for failure:
   ;;  OV_EREAD - A read from media returned an error.
   ;;  OV_ENOTVORBIS - Bitstream is not Vorbis data.
   ;;  OV_EVERSION - Vorbis version mismatch.
   ;;  OV_EBADHEADER - Invalid Vorbis bitstream header.
   ;;  OV_EFAULT - Internal logic fault; indicates a bug or heap/stack corruption.
(define info (ov_info vf -1))
(import (otus ffi))
(define info (vptr->vector info 16))
(define rate (extract-number info 8 4))
(print rate)

; пускай у нас будет 64 буфера
(define buffers (repeat 0 64))
(print buffers)
(alGenBuffers (length buffers) buffers)
(print buffers)

(define source (repeat 0 1))
(alGenSources 1 source)
(print source)

(define pcmout (make-bytevector 4096))

; заполним буфера файлом
(map (lambda (i)
      (let loop ((read (ov_read vf pcmout (size pcmout) 0 2 1 #false)))
         (if (eq? read 0) (begin ; file ended? restart :)
               (ov_clear vf)
               (ov_fopen (c-string environment-sound-file) vf)
               (loop (ov_read vf pcmout (size pcmout) 0 2 1 #f)))
            ; else put data into buffer
            (alBufferData i AL_FORMAT_MONO16 pcmout read rate))))
   buffers)
(alSourceQueueBuffers (car source) (length buffers) buffers)
(alSourcePlay (car source))

; а теперь запустим цикл обновления звуковых буферов
(fork-server 'music (lambda ()
(let this ((dictionary #empty))
(cond
   ; блок обработки сообщений
   ((check-mail) => (lambda (e) ; can be (and (eq? something 0) (check-mail)) =>
      (let*((sender msg e))
         ;(print "envelope: " envelope)
         (tuple-case msg
            ; low level interface:
            ((set key value)
               (this (put dictionary key value)))
            ((get key)
               (mail sender (get dictionary key #false))
               (this dictionary))
            ((debug)
               (mail sender dictionary)
               (this dictionary))

            ((shutdown)
               #true)

            (else
               (print-to stderr "Unknown music server command " msg)
               (this dictionary))))))
   (else
      ; проверим, а не закончились ли какие из буферов?
      (define count (list 0))
      (define pcmout (make-bytevector 4096))
      (alGetSourcei (car source) AL_BUFFERS_PROCESSED count)
      (unless (eq? (car count) 0)
         ; да, есть пустые буфера. надо дозаполнить
         (let ((released (repeat 0 (car count))))
            (alSourceUnqueueBuffers (car source) (car count) released)
            (map (lambda (i)
                  (let loop ((read (ov_read vf pcmout (size pcmout) 0 2 1 #false)))
                     (if (eq? read 0) (begin ; file ended? restart it :)
                           (ov_clear vf)
                           (ov_fopen (c-string environment-sound-file) vf)
                           (loop (ov_read vf pcmout (size pcmout) 0 2 1 #f)))
                        ; else put data into buffer
                        (alBufferData i AL_FORMAT_MONO16 pcmout read rate))))
               released)
            (alSourceQueueBuffers (car source) (car count) released)))
      (sleep 4)
      (this dictionary))))))
