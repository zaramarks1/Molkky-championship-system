package com.molkky.molkky.service;

import com.molkky.molkky.domain.Phase;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.domain.rounds.*;
import com.molkky.molkky.model.phase.PhaseListModel;
import com.molkky.molkky.model.phase.PhaseModel;
import com.molkky.molkky.repository.PhaseRepository;
import com.molkky.molkky.repository.TournamentRepository;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.Arrays;

@SpringBootTest
class PhaseServiceTest {

    @Autowired
    private PhaseService phaseService;

    @Autowired
    private TournamentRepository tournamentRepository;

    @Autowired
    private PhaseRepository phaseRepository;

    @Test
    void testClearTournamentPhase(){
        Pool pool = new Pool();
        Tournament tournament = new Tournament();
        tournament.setPhases(Arrays.asList(pool));
        pool.setTournament(tournament);

        tournament = tournamentRepository.save(tournament);
        phaseRepository.save(pool);
        phaseService.clearTournamentPhases(tournament);

        tournament = tournamentRepository.findById(tournament.getId());
        Assertions.assertEquals(0,tournament.getPhases().size());

    }

    @Test
    void testEditPoolInfo(){
        Pool pool = new Pool();
        Tournament tournament = new Tournament();
        tournament.setPhases(Arrays.asList(pool));
        pool.setTournament(tournament);

        tournamentRepository.save(tournament);
        pool = phaseRepository.save(pool);

        PhaseListModel phaseListModel = createPhaseModel(pool);

        tournament = phaseService.editPhasesInfo(phaseListModel);

        Assertions.assertEquals(1,tournament.getPhases().size());
        Assertions.assertEquals(2,tournament.getPhases().get(0).getNbSets());
        Assertions.assertNull(tournament.getPhases().get(0).getTimePhase());
        Assertions.assertNull(tournament.getPhases().get(0).getHourPhaseStart());
    }

    @Test
    void testEditFinnishInfo(){
        Finnish finnish = new Finnish();
        Tournament tournament = new Tournament();
        tournament.setPhases(Arrays.asList(finnish));
        finnish.setTournament(tournament);

        tournamentRepository.save(tournament);
        finnish = phaseRepository.save(finnish);

        PhaseListModel phaseListModel = createPhaseModel(finnish);

        tournament = phaseService.editPhasesInfo(phaseListModel);

        Assertions.assertEquals(1,tournament.getPhases().size());
        Assertions.assertEquals(2,tournament.getPhases().get(0).getNbSets());
        Assertions.assertNull(tournament.getPhases().get(0).getTimePhase());
        Assertions.assertNull(tournament.getPhases().get(0).getHourPhaseStart());
    }

    @Test
    void testEditKnockoutInfo(){
        Knockout knockout = new Knockout();
        Tournament tournament = new Tournament();
        tournament.setPhases(Arrays.asList(knockout));
        knockout.setTournament(tournament);

        tournamentRepository.save(tournament);
        knockout = phaseRepository.save(knockout);

        PhaseListModel phaseListModel = createPhaseModel(knockout);

        tournament = phaseService.editPhasesInfo(phaseListModel);

        Assertions.assertEquals(1,tournament.getPhases().size());
        Assertions.assertEquals(2,tournament.getPhases().get(0).getNbSets());
        Assertions.assertNull(tournament.getPhases().get(0).getTimePhase());
        Assertions.assertNull(tournament.getPhases().get(0).getHourPhaseStart());
    }

    @Test
    void testEditSimpleInfo(){
        SimpleGame simple = new SimpleGame();
        Tournament tournament = new Tournament();
        tournament.setPhases(Arrays.asList(simple));
        simple.setTournament(tournament);

        tournamentRepository.save(tournament);
        simple = phaseRepository.save(simple);

        PhaseListModel phaseListModel = createPhaseModel(simple);

        tournament = phaseService.editPhasesInfo(phaseListModel);

        Assertions.assertEquals(1,tournament.getPhases().size());
        Assertions.assertEquals(2,tournament.getPhases().get(0).getNbSets());
        Assertions.assertNull(tournament.getPhases().get(0).getTimePhase());
        Assertions.assertNull(tournament.getPhases().get(0).getHourPhaseStart());
    }

    @Test
    void testEditSwissInfo(){
        SwissPool swissPool = new SwissPool();
        Tournament tournament = new Tournament();
        tournament.setPhases(Arrays.asList(swissPool));
        swissPool.setTournament(tournament);

        tournamentRepository.save(tournament);
        swissPool = phaseRepository.save(swissPool);

        PhaseListModel phaseListModel = createPhaseModel(swissPool);

        tournament = phaseService.editPhasesInfo(phaseListModel);

        Assertions.assertEquals(1,tournament.getPhases().size());
        Assertions.assertEquals(2,tournament.getPhases().get(0).getNbSets());
        Assertions.assertNull(tournament.getPhases().get(0).getTimePhase());
        Assertions.assertNull(tournament.getPhases().get(0).getHourPhaseStart());
    }

    PhaseListModel createPhaseModel(Phase phase){
        PhaseModel phaseModel = new PhaseModel(phase);
        phaseModel.setPhaseType(phaseModel.getPhaseType());
        phaseModel.setId(phase.getId());
        phaseModel.setTimePhase("11:00");
        phaseModel.setHourPhaseStart("11:00");
        phaseModel.setNbSets(2);
        PhaseListModel phaseListModel = new PhaseListModel();
        phaseListModel.add(phaseModel);

        return phaseListModel;
    }

}