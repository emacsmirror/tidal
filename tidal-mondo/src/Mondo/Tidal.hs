{-# LANGUAGE ImportQualifiedPost #-}

module Mondo.Tidal where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Sound.Tidal.Control qualified as T
import Sound.Tidal.Core ((#), (|+|))
import Sound.Tidal.Params qualified as T
import Sound.Tidal.Pattern qualified as T
import Sound.Tidal.UI qualified as T

import Mondo.Params
import Mondo.Parser

-- * Control Patterns

sPat :: MondoParam String
sPat = (mkMondoParam "s" getString T.sound){combiner = (flip (#))}

nPat :: MondoParam T.Note
nPat = (mkMondoParam "n" getNote T.n){combiner = (|+|)}

mkScalePat :: (T.Pattern Int -> T.ControlPattern) -> MondoParam Int
mkScalePat scale = (mkMondoParam "scale" getInt scale){combiner = (|+|)}

-- * Grp Patterns

nColonPat :: MondoParam Double
nColonPat = MondoPat Nothing getDouble (T.pF "n") Nothing Nothing Nothing const Nothing

colonSoundPat :: MondoParam Double
colonSoundPat = (mkMondoParam "" getDouble (T.pF "n")){localExpr = Just $ MCommand "n-colon-pat"}

-- * Modifier Patterns

fastPat, slowPat :: MondoMod T.Time
fastPat = MondoMod getTime T.fast
slowPat = MondoMod getTime T.slow

iterPat :: MondoMod Int
iterPat = MondoMod getInt T.iter

maskPat :: MondoMod Bool
maskPat = MondoMod getBool T.mask

arpPat :: MondoMod String
arpPat = MondoMod getString T.arp

-- * Code gen...

int2Mods :: Map String (T.Pattern Int -> MondoMod Int)
int2Mods = Map.fromList $ map (\(n, f) -> (n, \p -> MondoMod getInt (f p))) funcs
  where
    funcs =
        [ ("splice", T.splice)
        , ("euclid", T.euclid)
        , ("euclidInv", T.euclidInv)
        , ("slice", T.slice)
        , ("chew", T.chew)
        ]

-- render list with: `grep -r tidal-core ":: Pattern Double -> ControlPattern" | sed 's/.*.hs:\([^ ]+\).*/  , ("\1", T.\1)/'`
doubleParams :: Map String (MondoParam Double)
doubleParams = Map.fromList $ map (\(n, f) -> (n, mkMondoDParam n getDouble f)) funcs
  where
    funcs =
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
        ]

-- render list with: `grep -r tidal-core ":: Pattern Int -> ControlPattern$" | grep -v "recv" | sed 's/.*.hs:\([^ ]*\).*/  , ("\1", T.\1)/'`
intParams :: Map String (MondoParam Int)
intParams = Map.fromList $ map (\(n, f) -> (n, mkMondoParam n getInt f)) funcs
  where
    funcs =
        [ ("nrpnn", T.nrpnn)
        , ("nrpnv", T.nrpnv)
        , ("channel", T.channel)
        , ("cut", T.cut)
        , ("octave", T.octave)
        , ("orbit", T.orbit)
        ]
