package com.molkky.molkky.service;

import com.molkky.molkky.domain.Match;
import com.molkky.molkky.domain.Phase;
import com.molkky.molkky.domain.Round;
import com.molkky.molkky.domain.rounds.Knockout;
import com.molkky.molkky.domain.rounds.Pool;
import com.molkky.molkky.domain.rounds.SimpleGame;
import com.molkky.molkky.domain.rounds.SwissPool;
import com.molkky.molkky.repository.PhaseRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import type.PhaseType;

import java.util.*;

@Service
public class PhaseService {

    @Autowired
    PhaseRepository phaseRepository;

    @Autowired
    PoolService poolService;

    @Autowired
    MatchService matchService;

    @Autowired
    SimpleGameService simpleGameService;

    @Autowired
    KnockoutService knockoutService;

    @Autowired
    SwissService swissService;

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

        for(Map.Entry<Round, List<Match>> entry : results.entrySet()){
            for(Match match : entry.getValue()){
                matchService.giveRandomCourtToMatch(match);
            }
        }

        return results ;

    }


}
