/* +======================================================================+ */
/* |    Copyright (c) 2005, 2017 Oracle and/or its affiliates.            | */
/* |                         All rights reserved.                         | */
/* |                           Version 12.0.0                             | */
/* +======================================================================+ */
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=plb \
REM dbdrv: checkfile(120.0.12010000.2=120.0.12020000.2):~PROD:~PATH:~FILE
SET VERIFY OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR EXIT FAILURE ROLLBACK;


create or replace package body gme_yield_calculation_pub AS
/*  $Header: GMEYMMPB.pls 120.0.12020000.2 2017/02/28 03:36:57 shalchen noship $    */

   g_debug               VARCHAR2 (5)  := NVL(fnd_profile.VALUE ('AFLOG_LEVEL'),-1);
   g_pkg_name   CONSTANT VARCHAR2 (30) := 'GME_YIELD_CALCULATION_PUB';


  FUNCTION material_in(p_batchstep_id IN gme_batch_steps.batchstep_id%type)
    RETURN NUMBER
    IS
    l_api_name           CONSTANT VARCHAR2 (30)  := 'MATERIAL_IN';
    l_return             NUMBER;
    l_count              NUMBER;
    
    
    CURSOR check_batch_step_id(v_batchstep_id NUMBER) IS
      SELECT count(1)
        FROM gme_batch_steps
       WHERE batchstep_id = v_batchstep_id; 
      
  BEGIN
    IF (g_debug <> -1) THEN
       gme_debug.log_initialize ('material_in');
    END IF;

    IF g_debug <= gme_debug.g_log_statement THEN
       gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                           || l_api_name);
    END IF;  
    
    OPEN check_batch_step_id(p_batchstep_id);
    FETCH check_batch_step_id INTO l_count;
    CLOSE check_batch_step_id; 
    
    IF l_count = 0 THEN
      fnd_message.set_name ('GME', 'GME_BATCHSTEP_NOT_FOUND');
      fnd_msg_pub.ADD;      
      RETURN -1;
    END IF;  
    
    l_return := gme_yield_calculation_pvt.material_in(p_batchstep_id => p_batchstep_id); 
    
    IF g_debug <= gme_debug.g_log_statement THEN
       gme_debug.put_line ('Caculation result of material_in:'|| l_return);
    END IF;     
    
    RETURN l_return;  
    
  EXCEPTION
    WHEN OTHERS THEN
      IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line (   'When others exception in '
                            || g_pkg_name
                            || '.'
                            || l_api_name
                            || ' Error is '
                            || SQLERRM);
      END IF;       
    
  END material_in;
    
    
    
  FUNCTION material_out(p_batchstep_id IN gme_batch_steps.batchstep_id%type)
    RETURN NUMBER
    IS
    l_api_name           CONSTANT VARCHAR2 (30)  := 'MATERIAL_OUT';
    l_return             NUMBER;
    l_count              NUMBER;
    
    
    CURSOR check_batch_step_id(v_batchstep_id NUMBER) IS
      SELECT count(1)
        FROM gme_batch_steps
       WHERE batchstep_id = v_batchstep_id;     
  BEGIN
    IF (g_debug <> -1) THEN
       gme_debug.log_initialize ('material_out');
    END IF;

    IF g_debug <= gme_debug.g_log_statement THEN
       gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                           || l_api_name);
    END IF;   
    
    OPEN check_batch_step_id(p_batchstep_id);
    FETCH check_batch_step_id INTO l_count;
    CLOSE check_batch_step_id; 
    
    IF l_count = 0 THEN
      fnd_message.set_name ('GME', 'GME_BATCHSTEP_NOT_FOUND');
      fnd_msg_pub.ADD;      
      RETURN -1;
    END IF;     
    
    l_return := gme_yield_calculation_pvt.material_out(p_batchstep_id => p_batchstep_id);  
    
    IF g_debug <= gme_debug.g_log_statement THEN
       gme_debug.put_line ('Caculation result of material_out:'|| l_return);
    END IF;        
    
    RETURN l_return;
    
  EXCEPTION
    WHEN OTHERS THEN   
      IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line (   'When others exception in '
                            || g_pkg_name
                            || '.'
                            || l_api_name
                            || ' Error is '
                            || SQLERRM);
      END IF;           
   
  END material_out;    
  
  
  FUNCTION get_inter_in(p_batchstep_id IN gme_batch_steps.batchstep_id%type)
    RETURN NUMBER
    IS
    l_api_name           CONSTANT VARCHAR2 (30)  := 'GET_INTER_IN';
    l_return             NUMBER;
    l_count              NUMBER;
    
    
    CURSOR check_batch_step_id(v_batchstep_id NUMBER) IS
      SELECT count(1)
        FROM gme_batch_steps
       WHERE batchstep_id = v_batchstep_id; 
           
  BEGIN
    IF (g_debug <> -1) THEN
       gme_debug.log_initialize ('get_inter_in');
    END IF;

    IF g_debug <= gme_debug.g_log_statement THEN
       gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                           || l_api_name);
    END IF;  
    
    OPEN check_batch_step_id(p_batchstep_id);
    FETCH check_batch_step_id INTO l_count;
    CLOSE check_batch_step_id; 
    
    IF l_count = 0 THEN
      fnd_message.set_name ('GME', 'GME_BATCHSTEP_NOT_FOUND');
      fnd_msg_pub.ADD;      
      RETURN -1;
    END IF;            
    
    l_return := gme_yield_calculation_pvt.get_inter_in(p_batchstep_id => p_batchstep_id);
    
    IF g_debug <= gme_debug.g_log_statement THEN
       gme_debug.put_line ('Caculation result of get_inter_in:'|| l_return);
    END IF;     
    
    RETURN l_return;
    
  EXCEPTION
    WHEN OTHERS THEN  
      IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line (   'When others exception in '
                            || g_pkg_name
                            || '.'
                            || l_api_name
                            || ' Error is '
                            || SQLERRM);
      END IF;           
    
  END get_inter_in;
  
  

  FUNCTION get_inter_out(p_batchstep_id IN gme_batch_steps.batchstep_id%type)
    RETURN NUMBER          
    IS
    l_api_name           CONSTANT VARCHAR2 (30)  := 'GET_INTER_OUT';
    l_return             NUMBER;
    l_count              NUMBER;
    
    
    CURSOR check_batch_step_id(v_batchstep_id NUMBER) IS
      SELECT count(1)
        FROM gme_batch_steps
       WHERE batchstep_id = v_batchstep_id;     
  BEGIN
    IF (g_debug <> -1) THEN
       gme_debug.log_initialize ('get_inter_out');
    END IF;

    IF g_debug <= gme_debug.g_log_statement THEN
       gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                           || l_api_name);
    END IF;
    
    OPEN check_batch_step_id(p_batchstep_id);
    FETCH check_batch_step_id INTO l_count;
    CLOSE check_batch_step_id; 
    
    IF l_count = 0 THEN
      fnd_message.set_name ('GME', 'GME_BATCHSTEP_NOT_FOUND');
      fnd_msg_pub.ADD;      
      RETURN -1;
    END IF;             
    
    l_return := gme_yield_calculation_pvt.get_inter_out(p_batchstep_id => p_batchstep_id);
    
    IF g_debug <= gme_debug.g_log_statement THEN
       gme_debug.put_line ('Caculation result of get_inter_out:'|| l_return);
    END IF;      
    
    RETURN l_return;
    
  EXCEPTION
    WHEN OTHERS THEN   
      IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line (   'When others exception in '
                            || g_pkg_name
                            || '.'
                            || l_api_name
                            || ' Error is '
                            || SQLERRM);
      END IF;              
    
  END get_inter_out;    
  
  
  
  FUNCTION scrap_out(p_batchstep_id IN gme_batch_steps.batchstep_id%type)
    RETURN NUMBER
    IS
    l_api_name           CONSTANT VARCHAR2 (30)  := 'SCRAP_OUT';
    l_return             NUMBER;
    l_count              NUMBER;
    
    
    CURSOR check_batch_step_id(v_batchstep_id NUMBER) IS
      SELECT count(1)
        FROM gme_batch_steps
       WHERE batchstep_id = v_batchstep_id;     
  BEGIN
    IF (g_debug <> -1) THEN
       gme_debug.log_initialize ('scrap_out');
    END IF;

    IF g_debug <= gme_debug.g_log_statement THEN
       gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                           || l_api_name);
    END IF;         
    
    OPEN check_batch_step_id(p_batchstep_id);
    FETCH check_batch_step_id INTO l_count;
    CLOSE check_batch_step_id; 
    
    IF l_count = 0 THEN
      fnd_message.set_name ('GME', 'GME_BATCHSTEP_NOT_FOUND');
      fnd_msg_pub.ADD;      
      RETURN -1;
    END IF;     
    
    l_return := gme_yield_calculation_pvt.scrap_out(p_batchstep_id => p_batchstep_id); 
    
    IF g_debug <= gme_debug.g_log_statement THEN
       gme_debug.put_line ('Caculation result of scrap_out:'|| l_return);
    END IF;      
    
    RETURN l_return;  
    
  EXCEPTION
    WHEN OTHERS THEN      
      IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line (   'When others exception in '
                            || g_pkg_name
                            || '.'
                            || l_api_name
                            || ' Error is '
                            || SQLERRM);
      END IF;       
    
  END scrap_out;      
  
  
  
  FUNCTION get_step_total_in_qty(p_batchstep_id IN gme_batch_steps.batchstep_id%type)
    RETURN NUMBER
    IS
    l_api_name           CONSTANT VARCHAR2 (30)  := 'GET_STEP_TOTAL_IN_QTY';
    l_return             NUMBER;
    l_count              NUMBER;
    
    
    CURSOR check_batch_step_id(v_batchstep_id NUMBER) IS
      SELECT count(1)
        FROM gme_batch_steps
       WHERE batchstep_id = v_batchstep_id;     
  BEGIN
    IF (g_debug <> -1) THEN
       gme_debug.log_initialize ('get_step_total_in_qty');
    END IF;

    IF g_debug <= gme_debug.g_log_statement THEN
       gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                           || l_api_name);
    END IF;   
    
    OPEN check_batch_step_id(p_batchstep_id);
    FETCH check_batch_step_id INTO l_count;
    CLOSE check_batch_step_id; 
    
    IF l_count = 0 THEN
      fnd_message.set_name ('GME', 'GME_BATCHSTEP_NOT_FOUND');
      fnd_msg_pub.ADD;      
      RETURN -1;
    END IF;           
    
    l_return := gme_yield_calculation_pvt.get_step_total_in_qty(p_batchstep_id => p_batchstep_id);
    
    IF g_debug <= gme_debug.g_log_statement THEN
       gme_debug.put_line ('Caculation result of get_step_total_in_qty:'|| l_return);
    END IF;     
    
    RETURN l_return;  
    
  EXCEPTION
    WHEN OTHERS THEN      
      IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line (   'When others exception in '
                            || g_pkg_name
                            || '.'
                            || l_api_name
                            || ' Error is '
                            || SQLERRM);
      END IF;       
  END get_step_total_in_qty;     
  
  
  
  
  
  FUNCTION get_step_total_out_qty(p_batchstep_id IN gme_batch_steps.batchstep_id%type)
    RETURN NUMBER 
    IS
    l_api_name           CONSTANT VARCHAR2 (30)  := 'GET_STEP_TOTAL_OUT_QTY';
    l_return             NUMBER;
    l_count              NUMBER;
    
    
    CURSOR check_batch_step_id(v_batchstep_id NUMBER) IS
      SELECT count(1)
        FROM gme_batch_steps
       WHERE batchstep_id = v_batchstep_id;     
  BEGIN
    IF (g_debug <> -1) THEN
       gme_debug.log_initialize ('get_step_total_out_qty');
    END IF;

    IF g_debug <= gme_debug.g_log_statement THEN
       gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                           || l_api_name);
    END IF; 
    
    OPEN check_batch_step_id(p_batchstep_id);
    FETCH check_batch_step_id INTO l_count;
    CLOSE check_batch_step_id; 
    
    IF l_count = 0 THEN
      fnd_message.set_name ('GME', 'GME_BATCHSTEP_NOT_FOUND');
      fnd_msg_pub.ADD;      
      RETURN -1;
    END IF;     
    
    l_return := gme_yield_calculation_pvt.get_step_total_out_qty(p_batchstep_id => p_batchstep_id);
    
    IF g_debug <= gme_debug.g_log_statement THEN
       gme_debug.put_line ('Caculation result of get_step_total_out_qty:'|| l_return);
    END IF;     
    
    RETURN l_return;          
    
  EXCEPTION
    WHEN OTHERS THEN
      IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line (   'When others exception in '
                            || g_pkg_name
                            || '.'
                            || l_api_name
                            || ' Error is '
                            || SQLERRM);
      END IF;         
        
  END get_step_total_out_qty;     
  
  
  
    /*###############################################################
  # name
  # insert_batch_step_quantity
  # synopsis
  # proc  insert_batch_step_quantity
  # description
  #     to insert GME Batch Step Quantity for step transfer details.
  # created by srmacha
  ###############################################################*/
  PROCEDURE insert_batch_step_quantity(
      p_batch_id                IN NUMBER,
      p_batch_step_quantity_tbl IN gme_api_pub.batch_step_quantity_tbl,
      p_commit           IN VARCHAR2 := fnd_api.g_false,
      x_return_status    OUT NOCOPY VARCHAR2,
      x_msg_count        OUT NOCOPY NUMBER,
      x_msg_data         OUT NOCOPY VARCHAR2) 
  IS

  l_api_name   CONSTANT VARCHAR2 (30)   := 'insert_batch_step_quantity';
  l_commit VARCHAR2(2);
  BEGIN
  FND_MSG_PUB.Initialize;
  x_return_status:=fnd_api.G_RET_STS_SUCCESS;
  l_commit :=p_commit;

  gme_api_main.insert_batch_step_quantity( p_batch_id => p_batch_id,
                p_batch_step_quantity_tbl =>p_batch_step_quantity_tbl,
                x_return_status =>x_return_status,
                x_msg_count   =>x_msg_count,
                x_msg_data   => x_msg_data,
                p_commit  => l_commit);

  IF l_commit = fnd_api.g_true THEN
  COMMIT;
  END IF;
 Exception
    WHEN OTHERS THEN
     IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||':'||'When others exception:'||SQLERRM);
     END IF;
     fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
     gme_common_pvt.count_and_get (x_count        => x_msg_count
                                  ,p_encoded      => fnd_api.g_false
                                  ,x_data         => x_msg_data);
      x_return_status := fnd_api.g_ret_sts_unexp_error;

   
  END insert_batch_step_quantity;
  
  
     /*###############################################################
  # name
  # update_batch_step_quantity
  # synopsis
  # proc  update_batch_step_quantity
  # description
  #     to update GME Batch Step Quantity for step transfer details.
  # created by 
  ###############################################################*/
   PROCEDURE update_batch_step_quantity(
      p_batch_id                IN NUMBER,
      p_batch_step_quantity_tbl IN gme_api_pub.batch_step_quantity_tbl,
      p_commit             IN VARCHAR2 := fnd_api.g_false,
      x_return_status    OUT NOCOPY VARCHAR2,
      x_msg_count        OUT NOCOPY NUMBER,
      x_msg_data         OUT NOCOPY VARCHAR2) IS

    l_api_name   CONSTANT VARCHAR2 (30)   := 'update_batch_step_quantity';
    l_commit VARCHAR2(2);
  BEGIN
    FND_MSG_PUB.Initialize;
    x_return_status:=fnd_api.G_RET_STS_SUCCESS;
    l_commit:= p_commit;

    gme_api_main.update_batch_step_quantity( p_batch_id => p_batch_id,
                p_batch_step_quantity_tbl =>p_batch_step_quantity_tbl,
                x_return_status =>x_return_status,
                x_msg_count   =>x_msg_count,
                x_msg_data   => x_msg_data,
                p_commit  => l_commit );
  
  IF l_commit = fnd_api.g_true THEN
  COMMIT;
  END IF;

  Exception
    WHEN OTHERS THEN
     IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||':'||'When others exception:'||SQLERRM);
     END IF;
     fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
     gme_common_pvt.count_and_get (x_count        => x_msg_count
                                  ,p_encoded      => fnd_api.g_false
                                  ,x_data         => x_msg_data);
      x_return_status := fnd_api.g_ret_sts_unexp_error;
            
  END update_batch_step_quantity;
    /*###############################################################
  # name
  # delete_batch_step_quantity
  # synopsis
  # procedure  delete_batch_step_quantity
  # description
  #     to delete GME Batch Step Quantity
  # created by 
  ###############################################################*/
  PROCEDURE delete_batch_step_quantity(
      p_batch_id                IN NUMBER,
      p_batch_step_quantity_tbl IN gme_api_pub.batch_step_quantity_tbl,
      p_commit             IN VARCHAR2 := fnd_api.g_false,
      x_return_status    OUT NOCOPY VARCHAR2,
      x_msg_count        OUT NOCOPY NUMBER,
      x_msg_data         OUT NOCOPY VARCHAR2) IS

  l_api_name   CONSTANT VARCHAR2 (30)   := 'delete_batch_step_quantity';
  l_commit VARCHAR2(2);
  BEGIN
    FND_MSG_PUB.Initialize;
    x_return_status:=fnd_api.G_RET_STS_SUCCESS;
    l_commit:= p_commit;

     gme_api_main.delete_batch_step_quantity( p_batch_id => p_batch_id,
                p_batch_step_quantity_tbl =>p_batch_step_quantity_tbl,
                x_return_status =>x_return_status,
                x_msg_count   =>x_msg_count,
                x_msg_data   => x_msg_data,
                 p_commit  => l_commit);

  IF l_commit = fnd_api.g_true THEN
  COMMIT;
  END IF;

   Exception
    WHEN OTHERS THEN
     IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||':'||'When others exception:'||SQLERRM);
     END IF;
     fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
     gme_common_pvt.count_and_get (x_count        => x_msg_count
                                  ,p_encoded      => fnd_api.g_false
                                  ,x_data         => x_msg_data);
      x_return_status := fnd_api.g_ret_sts_unexp_error;
            
  END delete_batch_step_quantity;

   /*************************************************************************/
   PROCEDURE ASSOCIATE_BATCHSTEP_ITEM (
      p_api_version              IN              NUMBER
     ,p_commit                   IN              VARCHAR2 := fnd_api.g_false
     ,p_org_code                 IN              VARCHAR2 := NULL
	 ,p_batch_no                 IN              VARCHAR2 := NULL
	 ,p_batchstep_no             IN              NUMBER := NULL
	 ,p_line_type             	 IN              NUMBER := NULL
	 ,p_line_no              	 IN              NUMBER
	 ,x_message_count            OUT NOCOPY      NUMBER
     ,x_message_list             OUT NOCOPY      VARCHAR2
     ,x_return_status            OUT NOCOPY      VARCHAR2
     )
   IS
      l_api_name       			CONSTANT VARCHAR2 (30)               := 'ASSOCIATE_BATCHSTEP_ITEM';
      l_return_status           VARCHAR2 (1);



	  x_eres_message_count      NUMBER;
      x_eres_message_list       VARCHAR2(2000);
      x_eres_return_status      VARCHAR2(10);

   BEGIN

	  gme_common_pvt.set_who;
      IF (g_debug <> -1) THEN
         gme_debug.log_initialize ('Associate Batch Step - Item');
      END IF;

      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
      END IF;

      /* Set the return status to success initially */
      x_return_status := fnd_api.g_ret_sts_success;

      IF NOT fnd_api.compatible_api_call (2.0
                                         ,p_api_version
                                         ,'create_batch'
                                         ,g_pkg_name) THEN
         RAISE fnd_api.g_exc_error;
      END IF;

      gme_common_pvt.g_error_count := 0;
      gme_common_pvt.g_setup_done :=
      gme_common_pvt.setup (p_org_id        => NULL
                           ,p_org_code      => p_org_code);


      IF g_debug <= gme_debug.g_log_statement THEN
         gme_debug.put_line ('Finished setup');
      END IF;

	  
	  
	  gme_api_main.ASSOCIATE_BATCHSTEP_ITEM (
		  p_api_version		=>	p_api_version
		 ,p_commit			=>	p_commit
		 ,p_org_code		=>	p_org_code
		 ,p_batch_no		=>	p_batch_no
		 ,p_batchstep_no	=>	p_batchstep_no
		 ,p_line_type		=>	p_line_type
		 ,p_line_no			=>	p_line_no
		 ,x_message_count	=>	x_message_count
		 ,x_message_list	=>	x_message_list
		 ,x_return_status	=>	x_return_status
		 );

	  
      IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line (   'Completed ' || l_api_name || ' at '
                             || TO_CHAR (SYSDATE, 'MM/DD/YYYY HH24:MI:SS') );
      END IF;
   EXCEPTION
      WHEN fnd_api.g_exc_error THEN
         x_return_status := fnd_api.g_ret_sts_error;
         gme_common_pvt.count_and_get (x_count        => x_message_count
                                      ,p_encoded      => fnd_api.g_false
                                      ,x_data         => x_message_list);
      WHEN OTHERS THEN
       IF g_debug <= gme_debug.g_log_unexpected THEN
          gme_debug.put_line(g_pkg_name||'.'||l_api_name||':'||'When others exception:'||SQLERRM);
       END IF;
       fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
       gme_common_pvt.count_and_get (x_count        => x_message_count
                                    ,p_encoded      => fnd_api.g_false
                                    ,x_data         => x_message_list);
        x_return_status := fnd_api.g_ret_sts_unexp_error;
   END ASSOCIATE_BATCHSTEP_ITEM;
   
   
   

  PROCEDURE update_batch_heat(
    p_batch_id             IN   NUMBER,
    p_batch_no             IN   VARCHAR2,
    p_organization_id      IN   NUMBER,
    p_heat_number          IN   VARCHAR2,
    p_commit               IN   VARCHAR2 := fnd_api.g_false,
    x_return_status        OUT  NOCOPY VARCHAR2,
    x_message_count        OUT  NOCOPY NUMBER,
    x_message_list         OUT  NOCOPY VARCHAR2
    ) IS
    
    l_api_name           CONSTANT VARCHAR2 (30)  := 'UPDATE_BATCH_HEAT';
    l_count              NUMBER;
    l_batch_id           NUMBER;
    
    batch_no_invalid     EXCEPTION;
    batch_no_duplicated  EXCEPTION;
   
    --check whether batch_no is valid 
    CURSOR check_batch_no_valid(v_batch_no VARCHAR2, v_organization_id NUMBER) IS
      SELECT count(1)
        FROM gme_batch_header
       WHERE batch_no = v_batch_no
         AND organization_id = v_organization_id;  
    
    --get batch_id     
    CURSOR get_batch_id(v_batch_no VARCHAR2, v_organization_id NUMBER) IS
      SELECT batch_id
        FROM gme_batch_header
       WHERE batch_no = v_batch_no
         AND organization_id = v_organization_id;            
    
  BEGIN
    IF (g_debug <> -1) THEN
       gme_debug.log_initialize ('update_batch_heat');
    END IF;

    IF g_debug <= gme_debug.g_log_statement THEN
       gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                           || l_api_name);
    END IF;

    /* Set the return status to success initially */
    x_return_status := fnd_api.g_ret_sts_success;

    fnd_msg_pub.initialize;
   
   /*derive batch_id based on batch_no  and organization_id*/
   IF p_batch_id IS NULL AND p_batch_no IS NOT NULL AND p_organization_id IS NOT NULL THEN   
     OPEN check_batch_no_valid(p_batch_no,p_organization_id);
     FETCH check_batch_no_valid INTO l_count;
     CLOSE check_batch_no_valid;
     
     IF l_count = 0 THEN
       RAISE batch_no_invalid;  
     
     ELSIF l_count > 1 THEN
       RAISE batch_no_duplicated; 
     ELSE
       OPEN get_batch_id(p_batch_no,p_organization_id);
       FETCH get_batch_id INTO l_batch_id;
       CLOSE get_batch_id;

     END IF;   
     
   ELSIF p_batch_id IS NOT NULL THEN
     l_batch_id := p_batch_id;
      
   END IF;  
   
   IF g_debug <= gme_debug.g_log_statement THEN
     gme_debug.put_line
        (   g_pkg_name
         || '.'
         || l_api_name
         || ' l_batch_id => '||l_batch_id||',p_heat_number=>'||p_heat_number);
   END IF;   
   

   /* Invoke main */
   gme_api_main.update_batch_heat( p_batch_id        => l_batch_id
                                  ,p_heat_number     => p_heat_number
                                  ,x_return_status   => x_return_status
                                  ,x_message_count   => x_message_count
                                  ,x_message_list    => x_message_list);
                                  
                                  
    IF g_debug <= gme_debug.g_log_statement THEN
       gme_debug.put_line
          (   g_pkg_name
           || '.'
           || l_api_name
           || ' Return status from gme_api_main.update_batch_heat is '
           || x_return_status);
    END IF;

    IF x_return_status <> fnd_api.g_ret_sts_success THEN
       ROLLBACK;
    ELSE
      IF p_commit = fnd_api.g_true THEN
        COMMIT;  
      END IF;  
    END IF;                                       
  
  EXCEPTION
    WHEN batch_no_invalid THEN
      x_return_status := fnd_api.g_ret_sts_error;
      gme_common_pvt.log_message ('GME_BATCH_NOT_FOUND');
      gme_common_pvt.count_and_get ( x_count        => x_message_count
                                    ,p_encoded      => fnd_api.g_false
                                    ,x_data         => x_message_list);       
      
    WHEN batch_no_duplicated THEN
      x_return_status := fnd_api.g_ret_sts_error;
      gme_common_pvt.log_message ('PM_DUPLICATE_BATCH_NO_ERR');
      gme_common_pvt.count_and_get ( x_count        => x_message_count
                                    ,p_encoded      => fnd_api.g_false
                                    ,x_data         => x_message_list);       
      
    WHEN OTHERS THEN
      ROLLBACK;
      IF g_debug <= gme_debug.g_log_unexpected THEN
        gme_debug.put_line(g_pkg_name||'.'||l_api_name||':'||'When others exception:'||SQLERRM);
      END IF;
      fnd_msg_pub.add_exc_msg (g_pkg_name, l_api_name);
      gme_common_pvt.count_and_get (x_count        => x_message_count
                                   ,p_encoded      => fnd_api.g_false
                                   ,x_data         => x_message_list);
      x_return_status := fnd_api.g_ret_sts_unexp_error;      
  
  END update_batch_heat;     
  
  
  
        
    
end gme_yield_calculation_pub;
/

COMMIT ;
EXIT;
