package com.molkky.molkky.service;

import com.molkky.molkky.domain.Round;
import com.molkky.molkky.domain.Team;
import com.molkky.molkky.domain.rounds.Pool;
import com.molkky.molkky.repository.RoundRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import type.PhaseType;

import java.util.ArrayList;
import java.util.List;

@Service
public class PoolService {

    @Autowired
    RoundRepository roundRepository;

    public void generateRounds(Pool pool){
        if(Boolean.FALSE.equals(pool.getRanking())){
            int nbPool = pool.getNbPools();

            List<Team> teamsOld = pool.getTournament().getTeams();

            List<Team> teamsNew = (List<Team>) teamsOld.stream()
                    .filter(team -> !team.isEliminated() );

            System.out.println(teamsNew.size());

            List<Round> rounds = new ArrayList<>();

            for(int i =1; i <= nbPool;i++){
                Round round = new Round();
                round.setPhase(pool);
                round.setTournament(pool.getTournament());
                round.setType(PhaseType.POOL);
                rounds.add(round);
            }

            int count =0;
            for(Team t : teamsNew){
                t.getRounds().add( rounds.get(count));
                rounds.get(count).getTeams().add(t);
                if(count ==3){
                    count = 0;
                }
                count++;
            }

            roundRepository.saveAll(rounds);
            System.out.println(rounds.size());
        }else {
            roundsWithRanking(pool);
        }
    }

    public void roundsWithRanking(Pool pool){
         pool.getNbPhase();
    }
}
