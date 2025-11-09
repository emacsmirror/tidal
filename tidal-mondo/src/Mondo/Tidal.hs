{-# LANGUAGE ImportQualifiedPost #-}

{- | This module regroups the tidal functions by their types.
The collection are named after the function's arguments, separated by '_':
* pA is 'Pattern a'
* pC is 'ControlPattern'
* pStr is 'Pattern String'
* pInt is 'Pattern Int'
* time is 'Time'

Examples:
* pStr_pC is 'Pattern String -> ControlPattern'
* time_pC_pC is 'Time -> ControlPattern -> ControlPattern'

Note: perhaps this should be part of tidal-core?
-}
module Mondo.Tidal where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Sound.Tidal.Control qualified as T
import Sound.Tidal.Core qualified as T
import Sound.Tidal.Params qualified as T
import Sound.Tidal.Pattern (ControlPattern, Note, Pattern, Time)
import Sound.Tidal.Pattern qualified as T
import Sound.Tidal.Stepwise qualified as T
import Sound.Tidal.UI qualified as T

pStr_pC :: Map String (Pattern String -> ControlPattern)
pStr_pC =
    Map.fromList
        [ ("sound", T.sound)
        , ("cc", T.cc)
        , ("nrpn", T.nrpn)
        , ("grain'", T.grain')
        , ("drum", T.drum)
        , ("bank", T.bank)
        , ("midicmd", T.midicmd)
        , ("toArg", T.toArg)
        , ("unit", T.unit)
        , ("vowel", T.vowel)
        , ("s", T.s)
        ]

int_pInt :: Map String (Int -> Pattern Int)
int_pInt =
    Map.fromList
        [ ("randrun", T.randrun)
        ]

pInt_pNum :: (Num a) => Map String (Pattern Int -> Pattern a)
pInt_pNum =
    Map.fromList
        [ ("irand", T.irand)
        ]

pENum_pENum :: (Enum a, Num a) => Map String (Pattern a -> Pattern a)
pENum_pENum =
    Map.fromList
        [ ("scan", T.scan)
        ]

pFrac :: (Fractional a) => Map String (Pattern a)
pFrac =
    Map.fromList
        [ ("sine", T.sine)
        , ("square", T.square)
        , ("cosine", T.cosine)
        , ("rand", T.rand)
        , ("perlin", T.perlin)
        ]

pENR_pENR :: (Enum a, Num a, Real a) => Map String (Pattern a -> Pattern a)
pENR_pENR =
    Map.fromList
        [ ("run", T.run)
        ]

pFracReal :: (Fractional a, Real a) => Map String (Pattern a)
pFracReal =
    Map.fromList
        [ ("saw", T.saw)
        , ("tri", T.tri)
        ]

realFrac_pA_pA :: (RealFrac a) => Map String (a -> Pattern a -> Pattern a)
realFrac_pA_pA =
    Map.fromList
        [ ("quantise", T.quantise)
        ]

pS_pA_pA :: Map String (Pattern String -> Pattern a -> Pattern a)
pS_pA_pA =
    Map.fromList
        [ ("arp", T.arp)
        ]

pInt_pOrd_pOrd :: (Ord a) => Map String (Pattern Int -> Pattern a -> Pattern a)
pInt_pOrd_pOrd =
    Map.fromList
        [ ("rot", T.rot)
        ]

pA_pA :: Map String (Pattern a -> Pattern a)
pA_pA =
    Map.fromList
        [ ("trigger", T.trigger)
        , ("qtrigger", T.qtrigger)
        , ("qt", T.qt)
        , ("ctrigger", T.ctrigger)
        , ("rtrigger", T.rtrigger)
        , ("ftrigger", T.ftrigger)
        , ("mono", T.mono)
        , ("splitQueries", T.splitQueries)
        , ("rev", T.rev)
        , ("filterOnsets", T.filterOnsets)
        , ("filterDigital", T.filterDigital)
        , ("filterAnalog", T.filterAnalog)
        , ("degrade", T.degrade)
        , ("brak", T.brak)
        , ("palindrome", T.palindrome)
        , ("stretch", T.stretch)
        , ("loopFirst", T.loopFirst)
        , ("arpeggiate", T.arpeggiate)
        , ("arpg", T.arpg)
        , ("rolled", T.rolled)
        , ("press", T.press)
        ]

pTime_pC_pC :: Map String (Pattern T.Time -> ControlPattern -> ControlPattern)
pTime_pC_pC =
    Map.fromList
        [ ("hurry", T.hurry)
        , ("loopAt", T.loopAt)
        ]

pTime_pTime_pA_pA :: Map String (Pattern T.Time -> Pattern T.Time -> Pattern a -> Pattern a)
pTime_pTime_pA_pA =
    Map.fromList
        [ ("ribbon", T.ribbon)
        , ("beat", T.beat)
        ]

pTime_int_pInt_pInt_pA_pA :: Map String (Pattern Time -> Int -> Pattern Int -> Pattern Int -> Pattern a -> Pattern a)
pTime_int_pInt_pInt_pA_pA =
    Map.fromList
        [ ("fit'", T.fit')
        ]

pC_pC :: Map String (ControlPattern -> ControlPattern)
pC_pC =
    Map.fromList
        [ ("ghost", T.ghost)
        ]

time_pA_pA :: Map String (T.Time -> Pattern a -> Pattern a)
time_pA_pA =
    Map.fromList
        [ ("fadeOut", T.fadeOut)
        , ("fadeIn", T.fadeIn)
        ]

int_time_pA_pA :: Map String (Int -> T.Time -> ControlPattern -> ControlPattern)
int_time_pA_pA =
    Map.fromList
        [ ("stutter", T.stutter)
        ]
time_pC_pC :: Map String (T.Time -> ControlPattern -> ControlPattern)
time_pC_pC =
    Map.fromList
        [ ("ghost'", T.ghost')
        , ("_pressBy", T._pressBy)
        ]

time_pC_ppC_pC :: Map String (Time -> ControlPattern -> [ControlPattern] -> ControlPattern)
time_pC_ppC_pC =
    Map.fromList
        [ ("weave", T.weave)
        ]

pApA_pA_pA :: Map String ((Pattern a -> Pattern a) -> Pattern a -> Pattern a)
pApA_pA_pA =
    Map.fromList
        [ ("superimpose", T.superimpose)
        ]

pTime_pApA_pA_pA :: Map String (Pattern Time -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)
pTime_pApA_pA_pA =
    Map.fromList
        [ ("off", T.off)
        ]

pInt_ppTime_pC_pC :: Map String (Pattern Int -> [Pattern Time] -> ControlPattern -> ControlPattern)
pInt_ppTime_pC_pC =
    Map.fromList
        [ ("smash", T.smash)
        ]

pInt_pApA_pA_pA :: Map String (Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)
pInt_pApA_pA_pA =
    Map.fromList
        [ ("every", T.every)
        , ("chunk", T.chunk)
        ]

-- sometimes and often are not strictly for control pattern, but it's simpler to restrict them here.
pCpC_pC_pC :: Map String ((ControlPattern -> ControlPattern) -> ControlPattern -> ControlPattern)
pCpC_pC_pC =
    Map.fromList
        [ ("sometimes", T.sometimes)
        , ("often", T.often)
        , ("jux", T.jux)
        ]

pDouble_pCpC_pC_pC :: Map String (Pattern Double -> (ControlPattern -> ControlPattern) -> ControlPattern -> ControlPattern)
pDouble_pCpC_pC_pC =
    Map.fromList
        [ ("juxBy", T.juxBy)
        ]

pDouble_pApA_pA_pA :: Map String (Pattern Double -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)
pDouble_pApA_pA_pA =
    Map.fromList
        [ ("sometimesBy", T.sometimesBy)
        , ("someCyclesBy", T.someCyclesBy)
        ]

pTime_pA_pA :: Map String (Pattern T.Time -> Pattern a -> Pattern a)
pTime_pA_pA =
    Map.fromList
        [ ("slowSqueeze", T.slowSqueeze)
        , ("sparsity", T.sparsity)
        , ("fastGap", T.fastGap)
        , ("densityGap", T.densityGap)
        , ("fast", T.fast)
        , ("fastSqueeze", T.fastSqueeze)
        , ("density", T.density)
        , ("slow", T.slow)
        , ("steptake", T.steptake)
        , ("stepdrop", T.stepdrop)
        , ("trunc", T.trunc)
        , ("linger", T.linger)
        , ("segment", T.segment)
        , ("discretise", T.discretise)
        , ("timeLoop", T.timeLoop)
        , ("swing", T.swing)
        , ("pressBy", T.pressBy)
        , ("ply", T.ply)
        ]

pBool_pA_pA :: Map String (Pattern Bool -> Pattern a -> Pattern a)
pBool_pA_pA =
    Map.fromList
        [ ("reset", T.reset)
        , ("restart", T.restart)
        , ("struct", T.struct)
        , ("mask", T.mask)
        ]

pC_pC_pC :: Map String (ControlPattern -> ControlPattern -> ControlPattern)
pC_pC_pC =
    Map.fromList
        [ ("interlace", T.interlace)
        ]

pInt_pA_pA :: Map String (Pattern Int -> Pattern a -> Pattern a)
pInt_pA_pA =
    Map.fromList
        [ ("repeatCycles", T.repeatCycles)
        , ("iter", T.iter)
        , ("iter'", T.iter')
        , ("substruct'", T.substruct')
        , ("stripe", T.stripe)
        , ("slowstripe", T.slowstripe)
        , ("shuffle", T.shuffle)
        , ("scramble", T.scramble)
        ]

pInt_pC_pC :: Map String (Pattern Int -> ControlPattern -> ControlPattern)
pInt_pC_pC =
    Map.fromList
        [ ("chop", T.chop)
        , ("spin", T.spin)
        , ("striate", T.striate)
        , ("gap", T.gap)
        , ("randslice", T.randslice)
        ]

pInt_pDouble_pC_pC :: Map String (Pattern Int -> Pattern Double -> ControlPattern -> ControlPattern)
pInt_pDouble_pC_pC =
    Map.fromList
        [ ("striateBy", T.striateBy)
        ]

pInt_pInt_pC_pC :: Map String (Pattern Int -> Pattern Int -> ControlPattern -> ControlPattern)
pInt_pInt_pC_pC =
    Map.fromList
        [ ("splice", T.splice)
        , ("euclid", T.euclid)
        , ("euclidInv", T.euclidInv)
        , ("slice", T.slice)
        , ("chew", T.chew)
        ]

pInt_pTime_pDouble_pC_pC :: Map String (Pattern Int -> Pattern Rational -> Pattern Double -> ControlPattern -> ControlPattern)
pInt_pTime_pDouble_pC_pC =
    Map.fromList
        [ ("echo", \p -> T.echo (toInteger <$> p))
        ]

-- render list with: `grep -r tidal-core ":: Pattern Double -> ControlPattern" | sed 's/.*.hs:\([^ ]+\).*/  , ("\1", T.\1)/'`
pDouble_pC :: Map String (Pattern Double -> ControlPattern)
pDouble_pC =
    Map.fromList
        [ ("accelerate", T.accelerate)
        , ("amp", T.amp)
        , ("attack", T.attack)
        , ("bandf", T.bandf)
        , ("bandq", T.bandq)
        , ("begin", T.begin)
        , ("binshift", T.binshift)
        , ("ccn", T.ccn)
        , ("ccv", T.ccv)
        , ("clhatdecay", T.clhatdecay)
        , ("coarse", T.coarse)
        , ("comb", T.comb)
        , ("control", T.control)
        , ("cps", T.cps)
        , ("crush", T.crush)
        , ("ctlNum", T.ctlNum)
        , ("ctranspose", T.ctranspose)
        , ("cutoff", T.cutoff)
        , ("cutoffegint", T.cutoffegint)
        , ("decay", T.decay)
        , ("degree", T.degree)
        , ("delay", T.delay)
        , ("delayfeedback", T.delayfeedback)
        , ("delaytime", T.delaytime)
        , ("detune", T.detune)
        , ("distort", T.distort)
        , ("djf", T.djf)
        , ("dry", T.dry)
        , ("dur", T.dur)
        , ("end", T.end)
        , ("enhance", T.enhance)
        , ("expression", T.expression)
        , ("fadeInTime", T.fadeInTime)
        , ("fadeTime", T.fadeTime)
        , ("frameRate", T.frameRate)
        , ("frames", T.frames)
        , ("freeze", T.freeze)
        , ("freq", T.freq)
        , ("from", T.from)
        , ("fshift", T.fshift)
        , ("fshiftnote", T.fshiftnote)
        , ("fshiftphase", T.fshiftphase)
        , ("gain", T.gain)
        , ("gate", T.gate)
        , ("harmonic", T.harmonic)
        , ("hatgrain", T.hatgrain)
        , ("hbrick", T.hbrick)
        , ("hcutoff", T.hcutoff)
        , ("hold", T.hold)
        , ("hours", T.hours)
        , ("hresonance", T.hresonance)
        , ("imag", T.imag)
        , ("kcutoff", T.kcutoff)
        , ("krush", T.krush)
        , ("lagogo", T.lagogo)
        , ("lbrick", T.lbrick)
        , ("lclap", T.lclap)
        , ("lclaves", T.lclaves)
        , ("lclhat", T.lclhat)
        , ("lcrash", T.lcrash)
        , ("legato", T.legato)
        , ("clip", T.clip)
        , ("leslie", T.leslie)
        , ("lfo", T.lfo)
        , ("lfocutoffint", T.lfocutoffint)
        , ("lfodelay", T.lfodelay)
        , ("lfoint", T.lfoint)
        , ("lfopitchint", T.lfopitchint)
        , ("lfoshape", T.lfoshape)
        , ("lfosync", T.lfosync)
        , ("lhitom", T.lhitom)
        , ("lkick", T.lkick)
        , ("llotom", T.llotom)
        , ("lock", T.lock)
        , ("loop", T.loop)
        , ("lophat", T.lophat)
        , ("lrate", T.lrate)
        , ("lsize", T.lsize)
        , ("lsnare", T.lsnare)
        , ("metatune", T.metatune)
        , ("midibend", T.midibend)
        , ("midichan", T.midichan)
        , ("miditouch", T.miditouch)
        , ("minutes", T.minutes)
        , ("modwheel", T.modwheel)
        , ("mtranspose", T.mtranspose)
        , ("nudge", T.nudge)
        , ("octaveR", T.octaveR)
        , ("octer", T.octer)
        , ("octersub", T.octersub)
        , ("octersubsub", T.octersubsub)
        , ("offset", T.offset)
        , ("ophatdecay", T.ophatdecay)
        , ("overgain", T.overgain)
        , ("overshape", T.overshape)
        , ("pan", T.pan)
        , ("panorient", T.panorient)
        , ("panspan", T.panspan)
        , ("pansplay", T.pansplay)
        , ("panwidth", T.panwidth)
        , ("partials", T.partials)
        , ("phaserdepth", T.phaserdepth)
        , ("phaserrate", T.phaserrate)
        , ("pitch1", T.pitch1)
        , ("pitch2", T.pitch2)
        , ("pitch3", T.pitch3)
        , ("polyTouch", T.polyTouch)
        , ("portamento", T.portamento)
        , ("progNum", T.progNum)
        , ("rate", T.rate)
        , ("real", T.real)
        , ("release", T.release)
        , ("resonance", T.resonance)
        , ("ring", T.ring)
        , ("ringdf", T.ringdf)
        , ("ringf", T.ringf)
        , ("room", T.room)
        , ("sagogo", T.sagogo)
        , ("sclap", T.sclap)
        , ("sclaves", T.sclaves)
        , ("scram", T.scram)
        , ("scrash", T.scrash)
        , ("seconds", T.seconds)
        , ("semitone", T.semitone)
        , ("shape", T.shape)
        , ("size", T.size)
        , ("slide", T.slide)
        , ("smear", T.smear)
        , ("songPtr", T.songPtr)
        , ("speed", T.speed)
        , ("squiz", T.squiz)
        , ("stepsPerOctave", T.stepsPerOctave)
        , ("stutterdepth", T.stutterdepth)
        , ("stuttertime", T.stuttertime)
        , ("sustain", T.sustain)
        , ("sustainpedal", T.sustainpedal)
        , ("timescale", T.timescale)
        , ("timescalewin", T.timescalewin)
        , ("to", T.to)
        , ("tomdecay", T.tomdecay)
        , ("tremolodepth", T.tremolodepth)
        , ("tremolorate", T.tremolorate)
        , ("triode", T.triode)
        , ("tsdelay", T.tsdelay)
        , ("uid", T.uid)
        , ("val", T.val)
        , ("vcfegint", T.vcfegint)
        , ("vcoegint", T.vcoegint)
        , ("velocity", T.velocity)
        , ("voice", T.voice)
        , ("waveloss", T.waveloss)
        , ("xsdelay", T.xsdelay)
        , ("voi", T.voi)
        , ("vco", T.vco)
        , ("vcf", T.vcf)
        , ("tremr", T.tremr)
        , ("tremdp", T.tremdp)
        , ("tdecay", T.tdecay)
        , ("sz", T.sz)
        , ("sus", T.sus)
        , ("stt", T.stt)
        , ("std", T.std)
        , ("sld", T.sld)
        , ("scr", T.scr)
        , ("scp", T.scp)
        , ("scl", T.scl)
        , ("sag", T.sag)
        , ("rel", T.rel)
        , ("por", T.por)
        , ("pit3", T.pit3)
        , ("pit2", T.pit2)
        , ("pit1", T.pit1)
        , ("phasr", T.phasr)
        , ("phasdp", T.phasdp)
        , ("ohdecay", T.ohdecay)
        , ("lsn", T.lsn)
        , ("lpq", T.lpq)
        , ("lpf", T.lpf)
        , ("loh", T.loh)
        , ("llt", T.llt)
        , ("lht", T.lht)
        , ("lfop", T.lfop)
        , ("lfoi", T.lfoi)
        , ("lfoc", T.lfoc)
        , ("lcr", T.lcr)
        , ("lcp", T.lcp)
        , ("lcl", T.lcl)
        , ("lch", T.lch)
        , ("lbd", T.lbd)
        , ("lag", T.lag)
        , ("hpq", T.hpq)
        , ("hpf", T.hpf)
        , ("hg", T.hg)
        , ("gat", T.gat)
        , ("fadeOutTime", T.fadeOutTime)
        , ("dt", T.dt)
        , ("dfb", T.dfb)
        , ("det", T.det)
        , ("delayt", T.delayt)
        , ("delayfb", T.delayfb)
        , ("ctfg", T.ctfg)
        , ("ctf", T.ctf)
        , ("chdecay", T.chdecay)
        , ("bpq", T.bpq)
        , ("bpf", T.bpf)
        , ("att", T.att)
        , -- strudel aliases
          ("dec", T.decay)
        , ("lpf", T.cutoff)
        , ("hpf", T.hcutoff)
        , ("vib", T.pF "vib")
        , ("vibmod", T.pF "vibmod")
        , ("distortvol", T.pF "distortvol")
        , ("pw", T.pF "pw")
        , ("lpe", T.pF "lpe")
        , ("lpa", T.pF "lpa")
        , ("lpd", T.pF "lpd")
        , ("lps", T.pF "lps")
        , ("lpr", T.pF "lpr")
        , ("hpe", T.pF "hpe")
        , ("hpa", T.pF "hpa")
        , ("hpd", T.pF "hpd")
        , ("hps", T.pF "hps")
        , ("hpr", T.pF "hpr")
        , ("bpe", T.pF "bpe")
        , ("bpa", T.pF "bpa")
        , ("bpd", T.pF "bpd")
        , ("bps", T.pF "bps")
        , ("bpr", T.pF "bpr")
        ]

-- render list with: `grep -r ":: Pattern Int -> ControlPattern$" tidal-core | grep -v "recv" | sed 's/.*.hs:\([^ ]*\).*/  , ("\1", T.\1)/'`
pInt_pC :: Map String (Pattern Int -> ControlPattern)
pInt_pC =
    Map.fromList
        [ ("nrpnn", T.nrpnn)
        , ("nrpnv", T.nrpnv)
        , ("channel", T.channel)
        , ("cut", T.cut)
        , ("octave", T.octave)
        , ("orbit", T.orbit)
        ]

pNote_pC :: Map String (Pattern Note -> ControlPattern)
pNote_pC =
    Map.fromList
        [ ("midinote", T.midinote)
        , ("n", T.n)
        , ("note", T.note)
        , ("up", T.up)
        , ("number", T.number)
        ]
