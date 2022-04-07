package com.molkky.molkky.scenario;

import com.molkky.molkky.domain.*;
import com.molkky.molkky.repository.*;
import lombok.Data;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.*;

@Data
@Service
public class PoolKnockoutScenario {
    @Autowired
    private TournamentRepository tournamentRepository;
    @Autowired
    private RoundRepository roundRepository;
    @Autowired
    private SwissPoolRepository swissPoolRepository;
    @Autowired
    private KnockoutRepository knockoutRepository;
    @Autowired
    private MatchRepository matchRepository;

    public PoolKnockoutScenario() {

    }

    public void init(Tournament tournament){
        tournamentRepository.save(tournament);
//        creer les rounds
        SwissPool swissPool = new SwissPool();

        Round swissRound = new Round();
        swissRound.setType("swissPool");
        swissRound.setNbTeams(4);
        swissRound.setTournament(tournament);
        roundRepository.save(swissRound);
        swissPool.setRound(swissRound);


        swissRound.setSwissPool(swissPool);
        swissPoolRepository.save(swissPool);

        Knockout knockout = new Knockout();

        Round knockOutRound = new Round();
        knockOutRound.setType("knockOut");
        knockOutRound.setNbTeams(2);
        knockOutRound.setTournament(tournament);
        roundRepository.save(knockOutRound);
        knockout.setRound(knockOutRound);

        knockOutRound.setKnockout(knockout);
        knockoutRepository.save(knockout);

        List<Round> roundsTournament = new ArrayList<>();
        roundsTournament.add(swissRound);
        roundsTournament.add(knockOutRound);
        tournament.setRounds(roundsTournament);

        tournamentRepository.save(tournament);
        System.out.println("Scénario initié");
    }

    public void start(Tournament tournament){
        System.out.println("Scénario démarré");
        generateMatchesForPool(tournament);
    }

//    Permet de recuperer les matchs de la phase actuel,
//    un poolknockout est fait de pools pouis d'un knock out
//    donc il recupere celui des pools d'abord
    public List<Match> getCurrentPhaseMatches(Tournament tournament){
        Integer indexPhase = tournament.getIndexPhase();
        List<Round> rounds = new ArrayList<>(tournament.getRounds());
        if(indexPhase == 0){
//            cas encore dans la pool
            return rounds.get(0).getPool().getMatches();
        } else{
//            cas dans le knock out
            return rounds.get(1).getPool().getMatches();
        }
    }

    public void setMatchScore(Match match, Integer scoreTeam1, Integer scoreTeam2, Tournament tournament){
        match.setScoreTeam1(scoreTeam1);
        match.setScoreTeam2(scoreTeam2);
        if(match.getScoreTeam1() == 50 || match.getScoreTeam2() == 50){
            match.setFinished(true);
            matchRepository.save(match);
            match.getSwissPool().setFinished(areAllMatchFinishedInPool(match.getSwissPool()));
            swissPoolRepository.save(match.getSwissPool());
            goToNextPhase(tournament);
        }
    }

    public void goToNextPhase(Tournament tournament){
        if(tournament.getIndexPhase() == 0){
            tournament.setIndexPhase(tournament.getIndexPhase() + 1);
            tournament = tournamentRepository.save(tournament);
            generateMatchesForPool(tournament);
        } else {
            System.out.println("Fin du tournoi");
            tournament.setFinished(true);
            tournamentRepository.save(tournament);
        }
    }

    public Boolean areAllMatchFinishedInPool(SwissPool swissPool){
        Boolean finished = true;
        for (Match match : swissPool.getMatches()) {
            finished = match.getFinished();
        }
        return finished;
    }

//    generer les matchs pour la pool en cours
    public void generateMatchesForPool(Tournament tournament){
        List<Match> matches = new ArrayList<>();
        List<Team> teams = new ArrayList<>(tournament.getTeams());
//        pour la pool il faut un match contre tout le monde
        if(tournament.getIndexPhase() == 0){
            for(int i = 0; i < tournament.getRounds().get(0).getNbTeams(); i++){
                for(int y = 0; y < tournament.getRounds().get(0).getNbTeams(); y++){
                    if(i != y){
                        Match nvMatch = new Match();
                        Set<Team> teamsMatch = new HashSet<>();
                        teamsMatch.add(teams.get(i));
                        teamsMatch.add(teams.get(y));
                        nvMatch.setTeams(teamsMatch);
                        nvMatch.setSwissPool(tournament.getRounds().get(0).getSwissPool());
                        nvMatch = matchRepository.save(nvMatch);
                        matches.add(nvMatch);
                    }
                }
            }
            tournament.getRounds().get(0).getSwissPool().setMatches(matches);
            roundRepository.save(tournament.getRounds().get(0));
        }
        else if(tournament.getIndexPhase() == 1){
            ArrayList<Team> winningTeams = new ArrayList<>(getWinningTeamsFromPool(tournament));
            for(int i = 0; i < tournament.getRounds().get(1).getNbTeams(); i++){
                for(int y = 0; y < tournament.getRounds().get(1).getNbTeams(); y++){
                    if(i != y){
                        Match nvMatch = new Match();
                        Set<Team> teamsMatch = new HashSet<>();
                        teamsMatch.add(winningTeams.get(i));
                        teamsMatch.add(winningTeams.get(y));
                        nvMatch.setTeams(teamsMatch);
                        nvMatch = matchRepository.save(nvMatch);
                        matches.add(nvMatch);
                    }
                }
            }
            tournament.getRounds().get(1).getKnockout().setMatches(matches);
            roundRepository.save(tournament.getRounds().get(1));
        }
    }

    public List<Team> getWinningTeamsFromPool(Tournament tournament){
        List<Team> teams = new ArrayList<>(tournament.getTeams());
        teams.sort(Comparator.comparing(Team::getNbWins));
        ArrayList<Team> returnedTeams = new ArrayList<>();
        returnedTeams.add(teams.get(teams.size() - 1));
        returnedTeams.add(teams.get(teams.size() - 2));
        return returnedTeams;
    }

}
