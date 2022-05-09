package com.molkky.molkky.service;

import com.molkky.molkky.domain.Match;
import com.molkky.molkky.domain.Phase;
import com.molkky.molkky.domain.Round;
import com.molkky.molkky.domain.rounds.Pool;
import com.molkky.molkky.repository.PhaseRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.HashMap;
import java.util.List;

@Service
public class PhaseService {

    @Autowired
    PhaseRepository phaseRepository;

    @Autowired
    PoolService poolService;

    public HashMap<Round, List<Match>> generate(String id){

        Phase phase = phaseRepository.findById(Integer.valueOf(id));

        if (phase instanceof Pool){
            Pool pool = (Pool) phase;
            return poolService.generateRounds(pool);
        }

        return null;

    }


}
