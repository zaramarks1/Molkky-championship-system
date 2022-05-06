package com.molkky.molkky.service;

import com.molkky.molkky.domain.Phase;
import com.molkky.molkky.domain.rounds.Pool;
import com.molkky.molkky.repository.PhaseRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class PhaseService {

    @Autowired
    PhaseRepository phaseRepository;

    @Autowired
    PoolService poolService;

    public void generate(String id){

        Phase phase = phaseRepository.getById(id);

        if (phase instanceof Pool){
            Pool pool = (Pool) phase;
            poolService.generateRounds(pool);
        }

    }


}
