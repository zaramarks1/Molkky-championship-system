package com.molkky.molkky.service;

import com.molkky.molkky.domain.Match;
import com.molkky.molkky.domain.Round;
import com.molkky.molkky.domain.Team;
import com.molkky.molkky.domain.rounds.Pool;
import com.molkky.molkky.repository.MatchRepository;
import com.molkky.molkky.repository.RoundRepository;
import com.molkky.molkky.repository.TeamRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import type.PhaseType;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.stream.*;

@Service
public class PoolService {

    @Autowired
    RoundRepository roundRepository;

    @Autowired
    MatchRepository matchRepository;

    @Autowired
    TeamRepository teamRepository;

    public HashMap<Round, List<Match>> generateRounds(Pool pool){
        HashMap<Round, List<Match>> results = new HashMap();

        if(Boolean.FALSE.equals(pool.getRanking())){
            int nbPool = pool.getNbPools();

            List<Team> teamsOld = pool.getTournament().getTeams();

            List<Team> teamsNew = teamsOld.stream()
                    .filter(team -> !team.isEliminated())
                            .collect(Collectors.toList());

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
                count++;

                if(count == nbPool ){
                    count = 0;
                }
            }

            roundRepository.saveAll(rounds);


            List<Match> matches;

            for (Round r : rounds){
              int nbMatch =  r.getTeams().size() -1;
              matches = new ArrayList<>();
              for(int i =0; i < nbMatch;i++){
                  Team team1 = r.getTeams().get(i);
                  for(int j=i+1;j<=nbMatch;j++){
                      Team team2 = r.getTeams().get(j);
                      Match match = new Match();
                      match.setRound(r);
                      match.getTeams().add(team1);
                      match.getTeams().add(team2);

                      team1.getMatchs().add(match);
                      team2.getMatchs().add(match);

                      matches.add(match);

                      matchRepository.save(match);
                      teamRepository.save(team1);
                      teamRepository.save(team2);
                  }
              }
              results.put(r, matches);
            }
        }else {
            roundsWithRanking(pool);
        }

        return results;
    }

    public void roundsWithRanking(Pool pool){
         pool.getNbPhase();
    }
}
