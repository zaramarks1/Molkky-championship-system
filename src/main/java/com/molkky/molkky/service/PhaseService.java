package com.molkky.molkky.service;

import com.molkky.molkky.domain.Match;
import com.molkky.molkky.domain.Phase;
import com.molkky.molkky.domain.Round;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.domain.rounds.*;
import com.molkky.molkky.model.phase.PhaseListModel;
import com.molkky.molkky.model.phase.PhaseModel;
import com.molkky.molkky.repository.PhaseRepository;
import com.molkky.molkky.repository.TournamentRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.*;

@Service
public class PhaseService {

    @Autowired
    PhaseRepository phaseRepository;

    @Autowired
    PoolService poolService;

    @Autowired
    SimpleGameService simpleGameService;

    @Autowired
    KnockoutService knockoutService;

    @Autowired
    SwissService swissService;

    @Autowired
    TournamentRepository tournamentRepository;

    @Autowired
    RoundService roundService;

    public Map<Round, List<Match>> generate(String id){

        Map<Round, List<Match>> results = new HashMap<>();
        Phase phase = phaseRepository.findById(Integer.valueOf(id));

        if (phase instanceof Pool){
            Pool pool = (Pool) phase;
            results = poolService.generateRounds(pool);
        }else if(phase instanceof SimpleGame){
            SimpleGame simpleGame = (SimpleGame) phase;
            results = simpleGameService.generateRounds(simpleGame);
        }else if( phase instanceof Knockout){
            Knockout knockout = (Knockout) phase;
            results = knockoutService.generateRounds(knockout);
        }else if (phase instanceof SwissPool){
            SwissPool swissPool = (SwissPool) phase;
            results = swissService.generateRounds(swissPool);
        }

        return results ;

    }

    public void clearTournamentPhases(Tournament tournament){
        List<Phase> phases = tournament.getPhases();
        for(Phase phase : phases){
            phase.setTournament(null);
        }
        phaseRepository.saveAll(phases);
        phases = phaseRepository.findByTournament(null);
        phaseRepository.deleteAll(phases);
    }

    public Tournament editPhasesInfo(PhaseListModel phasesModel){
        List<Phase> phases = new ArrayList<>();
        for(PhaseModel phase : phasesModel.getPhases()){
            switch (phase.getPhaseType()){
                case POOL:
                    Pool phasePool = (Pool) phaseRepository.findById(phase.getId());
                    phasePool.editInfoPool(phase);
                    phases.add(phasePool);
                    break;
                case FINNISH:
                    Finnish phaseFinnish = (Finnish) phaseRepository.findById(phase.getId());
                    phaseFinnish.editInfoFinnish(phase);
                    phases.add(phaseFinnish);
                    break;
                case KNOCKOUT:
                    Knockout phaseKnockout = (Knockout) phaseRepository.findById(phase.getId());
                    phaseKnockout.editInfoKnockout(phase);
                    phases.add(phaseKnockout);
                    break;
                case SWISSPOOL:
                    SwissPool phaseSwiss = (SwissPool) phaseRepository.findById(phase.getId());
                    phaseSwiss.editInfoSwiss(phase);
                    phases.add(phaseSwiss);
                    break;
                case SIMPLEGAME:
                    SimpleGame phaseSimple = (SimpleGame) phaseRepository.findById(phase.getId());
                    phaseSimple.editInfoSimple(phase);
                    phases.add(phaseSimple);
                    break;
                default:
                    break;
            }
        }
        phaseRepository.saveAll(phases);
        Tournament t = tournamentRepository.findById(phases.get(0).getTournament().getId());
        t.setPhases(phases);
        return tournamentRepository.save(t);
    }
}
